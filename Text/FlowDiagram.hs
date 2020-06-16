{-|
Converts flow diagrams to the Graphviz (Dot) files for subsequent rendering
into nice pictures.
-}
module Text.FlowDiagram ( flow2dot
                        , parseFlow
                        , parseFlowFromFile
                        , showFlow
                        , Flow(..)
                        ) where

import qualified Text.Dot as D
import Control.Monad.State (StateT, evalStateT, gets, modify, lift, zipWithM, zipWithM_, foldM, when)
import qualified Data.Map as M (Map, empty, lookup, insert, union, fromList)
import Data.List (intercalate, unfoldr, splitAt, findIndex, nub, sort)
import System.IO (readFile)
import Data.Maybe (catMaybes, isJust, fromMaybe)
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec hiding (State)

{-
Idea: In order to draw sequence (flow) diagram using graphviz we can use directed layout (dot) to
generate "skeleton" of the diagram and draw message lines and action boxes
-}


type Swimline = String

-- | Flow could include messages and actions, one item per source line
data Flow = Msg Swimline Swimline String
          -- ^ Message (from, to, message text). Syntax in the source file: @from -> to: message text@
          | Action Swimline String
          -- ^ Action (actor, message text). Syntax in the source file: @actor: message text@
          | Order [Swimline]
          -- ^ Tries to put swimlines in the specified order. Syntax: @order swimline1 swimline2 ...@
            deriving (Eq,Show)

isOrder (Order _) = True
isOrder _         = False

isMsg (Msg _ _ _) = True
isMsg _           = False

names flow = nub $ flip concatMap flow $ \elt ->
  case elt of
    Msg from to _ -> [from,to]
    Action a _ -> [a]
    Order _ -> []
    

-- | State of the diagram builder
data DiagS = DiagS { nodes :: M.Map (Int,Swimline) D.NodeId
                   -- ^ all the nodes of the graph, indexed by tier and swimline name
                   }

type Diagram = StateT DiagS D.Dot

-- | 'flow2dot' take a list of flow diagram items (`Flow') and converts them to Graphviz code
flow2dot :: [Flow] -> String
flow2dot flow = 
  ("strict "++) $ D.showDot $ evalStateT (flow2dot' flow) (DiagS M.empty)
    -- NB: "strict" is VERY important here
    -- Without it, "dot" segfaults while rendering diagram (dot 2.12)

flow2dot' :: [Flow] -> Diagram ()
flow2dot' flow = do
  -- Avoid curved edges at all cost
  attribute ("splines","line")
  
  let order = case [ ns | Order ns <- flow ] of
                []     -> Nothing
                -- Only the first Order directive would be taken into account
                (ns:_) -> Just ns
      flow' = filter (not.isOrder) flow
      swimlines = names flow'

  -- check order
  case order of
    Just order' | sort order' /= sort swimlines -> do
      error "order statement must mention all swimlines"
    _ -> return ()
  
  -- Create nodes for flow elements
  flip mapM_ (zip [0..] flow') $ \(tier,elt) -> do
    namedNodes <- flowElement2dot tier order elt
    addNodes $ M.fromList $ [ ((tier,swimline),node) | (swimline,node)<-namedNodes ]

  -- Add nodes on all the remaining places on all the swimlines
  -- We don't add nodes on the same tier as Msg elements to avoid dot trying to route
  -- Msg edge around those invisible support nodes.
  flip mapM_ [ (tier, sline) | sline <- swimlines
                             , (tier,elt) <-zip [0..] flow'
                             , not (isMsg elt) ] $ \(tier,sline) -> do
    n <- findNode tier sline
    case n of
      Just _ -> return ()
      Nothing -> do
          id <- invisNode
          addNodes (M.fromList [((tier,sline),id)])

  -- create swimline headers
  headers <- flip mapM (fromMaybe swimlines order) $ \sline ->
    node [ ("label", mkHeader sline)
         , ("shape","box")
         ]
  same (headers)

  -- apply order, if any
  when (isJust order) $ do
    zipWithM_ (\h1 h2 -> edge h1 h2 [ ("style","invis")
                                    , ("weight","1") -- this edge is weak compared to verticals
                                    ])
      headers (drop 1 headers)
  
  -- connect all nodes on all swimlines
  -- first, lets group them by tier
  nodesByTier <- flip mapM [ tier | (tier,_) <-zip [0..] flow' ] $ \tier ->
    mapM (findNode tier) [ line | line <- swimlines ]

  -- then, we descend through the tiers connectin nodes to the previous node on
  -- the same swimline, starting from headers
  _ <- foldM (\prevNodes thisTier -> do
          same $ catMaybes thisTier
          zipWithM (\prevNode maybeThisNode ->
                      case maybeThisNode of
                        Just thisNode -> do
                          edge prevNode thisNode
                            [("style","dotted")
                            ,("arrowhead","none")
                            ,("weight","1000") -- these edges want to be
                                               -- very vertical
                            ]
                          return thisNode
                        Nothing -> return prevNode)
            prevNodes thisTier)
         headers nodesByTier
  return ()

flowElement2dot :: Int -> Maybe [String] -> Flow -> Diagram [(String,D.NodeId)]
-- Generate nodes for message/action on all required swimlines
flowElement2dot tier _ (Action actor message) = do
  l <- mkLabel tier message
  a <- node [("style","filled"),("shape","plaintext"),("label",l)]
  return [(actor,a)]

flowElement2dot tier order (Msg from to message) = do
  f    <- invisNode
  t    <- invisNode

  l <- mkLabel tier message

  let (f',t',attrs) = 
        if order == Nothing
           then (f,t,[])
           else let (Just sls) = order
                    in case (findIndex (==from) sls, findIndex (==to) sls) of
                         (Just x, Just y) -> if x>y then (t,f,[("dir","back")]) else (f,t,[])
                         _                -> (f,t,[])

  edge f' t' $ [("label",l)
               ,("constraint","false") -- avoid pushing recipient node down
               ,("labelfloat","true") -- be a bit sloppy with label placement
               ] ++ attrs
  return [(from,f),(to,t)]
    
-- Order setting is done in Msg processing
flowElement2dot _ _ (Order _) = return []

mkLabel :: Int -> String -> Diagram String
mkLabel tier lbl = do
  return $ if null lbl then ""
                       else (show (tier+1) ++ ": " ++ reflow lbl)

invisNode :: Diagram D.NodeId
invisNode = node [("style","invis"),("shape","point")]

reflow :: String -> String
reflow str = intercalate "\n" $ map unwords $ splitInto words_in_row w
      where w = words str
            z = length w
            rows = z*height `div` (height+width)
            words_in_row = rows*width `div` height
            chunk _ []  = Nothing
            chunk 0 lst = Just (lst, [])
            chunk n lst = Just $ splitAt n lst
            splitInto n = unfoldr (chunk n)
            -- FIXME: for now, you have to hardcode desired width/height ratio
            width=3
            height=1

mkHeader :: String -> String
mkHeader = map remove_underscore
  where
    remove_underscore '_' = ' '
    remove_underscore x   = x

------------------------------
-- State access/modify helpers
------------------------------

addNodes :: M.Map (Int,String) D.NodeId -> Diagram ()
addNodes newNodes = do
  modify (\e -> e {nodes = M.union (nodes e) newNodes})

findNode :: Int -> String -> Diagram (Maybe D.NodeId)
findNode tier line = do
  ns <- gets nodes
  return $ M.lookup (tier, line) ns

------------------------------------------------
-- Lifting Text.Dot functions to the State monad
------------------------------------------------
same = lift . D.same
node = lift . D.node
edge f t args = lift $ D.edge f t args
attribute = lift . D.attribute

---------
-- Parser
---------
-- | Parse specified file and return Flow Diagram contained therein.
-- All syntax errors are thrown via 'error'
parseFlowFromFile :: FilePath -> IO [Flow]
parseFlowFromFile fname = do
  raw <- readFile fname
  return $ parseFlow fname raw

-- | Parse given string and return Flow Diagram contained therein.
-- All syntax errors are thrown via 'error'
parseFlow :: String -> String -> [Flow]
parseFlow _     ""  = []
parseFlow fname str =
  case parse document fname str of
       Left err   -> error $ unlines [ "Input:", str, "Error:", show err]
       Right flow -> flow

document :: GenParser Char st [Flow]
document = do
  whitespace
  fl <- many flowLine
  eof
  return $ catMaybes fl

flowLine, parseMsg, parseAction :: GenParser Char st (Maybe Flow)
flowLine = try parseOrder <|> try parseMsg <|> try parseAction <|> parseComment <|> parseBlank
parseOrder = do string "order"
                is <- identifier `manyTill` newline
                return $ Just $ Order is
parseMsg = do f <- identifier; string "->"; t <- identifier; m <- optionalMessage
              return $ Just $ Msg f t (trim m)
parseAction = do s <- identifier; string ":"; a <- anything
                 return $ Just $ Action s (trim a)
parseBlank = do whitespace; newline; return Nothing
parseComment = do whitespace; string "#"; anything; return Nothing

optionalMessage = do
  m <- option "" (try (do {string ":"; anything}))
  optional newline
  return m

identifier, whitespace, anything :: GenParser Char st String
identifier = do whitespace; i <- many1 (alphaNum <|> oneOf "_"); whitespace
                return i
whitespace = many $ oneOf " \t"
anything = try (anyChar `manyTill` newline) <|> many1 anyChar

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Print element of the flow diagram as String
showFlow :: Flow -> String
showFlow (Order sl)   = "order " ++ intercalate " " sl
showFlow (Msg f t m)  = unwords [ f, " -> ", t, ":", m ]
showFlow (Action s a) = unwords [ s, ":", a ]

