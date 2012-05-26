{-# LANGUAGE CPP #-}
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
import Control.Monad.State (StateT, evalStateT, gets, modify, lift)
import qualified Data.Map as M (Map, empty, lookup, insert)
import Data.List (intercalate, unfoldr, splitAt, findIndex)
#ifndef NATIVEUTF8
import Prelude hiding (readFile)
import System.IO.UTF8 (readFile)
#endif
import Data.Maybe (catMaybes)
import Data.Char (isSpace)
import Test.QuickCheck
import Control.Monad (liftM, liftM2, liftM3)
import Text.ParserCombinators.Parsec hiding (State)

{-
Idea: In order to draw sequence (flow) diagram using graphviz we can use directed layout (dot) to
generate "skeleton" of the diagram and draw message lines and action boxes

Diagram could look like this:
strict digraph SeqDiagram
{
  { // Those are swimline heads
    rank=same
    actor [label="Some actor"];
    system [label="Some system"];
  }
  { //tier1
    rank=same
    node[style=invis,shape=point];
    tier1; // this is an "anchor" for 1st diagram tier
    actor1; // this is a 1st point on "actor" swimline
    system1; // this is a 1st point on "system" swimline
  }
  { //tier2
    rank=same
    node[style=invis,shape=point];
    tier2; // anchor for 2nd diagram tier
    actor2; // this is a 2nd point on "actor" swimline
    system2; // this is a 2nd point on "system" swimline
  }
  // Main body

  // Tiers ordering. We link "anchor" nodes and Dot will do the rest
  tier1 -> tier2;

  // Actual messages. Note the "constraint=false"
  actor1 -> system1[label="xxx", constraint=false];
  system2 -> actor2[label="yyy", constraint=false];
}
-}

-- | Flow could include messages and actions, one item per source line
data Flow = Msg String String String
          -- ^ Message (from, to, message text). Syntax in the source file: @from -> to: message text@
          | Action String String
          -- ^ Action (actor, message text). Syntax in the source file: @actor: message text@
          | Order [String]
          -- ^ Tries to put swimlines in the specified order. Syntax: @order swimline1 swimline2 ...@
            deriving (Eq,Show)

-- | State of the diagram builder
data DiagS = DiagS { swimlines::M.Map String D.NodeId
                   -- ^ name of the swimline, ID of the last node on it
                   , numTier :: Int
                   -- ^ number of the next diagram tier
                   , headings :: [D.NodeId]
                   -- ^ IDs of all "swimline start" nodes
                   }

type Diagram = StateT DiagS D.Dot

-- | 'flow2dot' take a list of flow diagram items (`Flow') and converts them to Graphviz code
flow2dot :: [Flow] -> String
flow2dot flow = 
  ("strict "++) $ D.showDot $ evalStateT (flow2dot' flow) (DiagS M.empty 1 [])
    -- NB: "strict" is VERY important here
    -- Without it, "dot" segfaults while rendering diagram (dot 2.12)

flow2dot' :: [Flow] -> Diagram ()
flow2dot' flow = do
  let order = case [ ns | Order ns <- flow ] of
                []     -> Nothing
                -- Only the first Order directive would be taken into account
                (ns:_) -> Just ns
  mapM_ (flowElement2dot order) flow
  hs <- gets headings
  same hs

flowElement2dot :: Maybe [String] -> Flow -> Diagram ()
-- Make a graph block where swimline nodes for the current tier will be put.
-- Populate tier with "tier anchor" node
-- Generate nodes for message/action on all required swimlines
-- Connect generated nodes, if necessary
-- Connect tier to previous, which will ensure that tiers are ordered properly
flowElement2dot _ (Action actor message) = do
  tier <- invisNode
  l <- mkLabel message
  a <- node [("style","filled"),("shape","plaintext"),("label",l)]
  same [tier,a]
  connectToPrev actor a
  connectToPrev "___tier" tier
  incTier

flowElement2dot order (Msg from to message) = do
  tier <- invisNode
  f    <- invisNode
  t    <- invisNode
  same [f,t,tier]

  l <- mkLabel message

  connectToPrev from f
  connectToPrev to t
  connectToPrev "___tier" tier
  let (f',t',attrs) = 
        if order == Nothing
           then (f,t,[])
           else let (Just sls) = order
                    in case (findIndex (==from) sls, findIndex (==to) sls) of
                         (Just x, Just y) -> if x>y then (t,f,[("dir","back")]) else (f,t,[])
                         _                -> (f,t,[])

  edge f' t' $ [("label",l)] ++ attrs
  incTier
    
-- Order setting is done in Msg processing
flowElement2dot _ (Order _) = return ()



mkLabel :: String -> Diagram String
mkLabel lbl = do
  t <- gets numTier
  return $ if null lbl then show t
                       else (show t ++ ": " ++ reflow lbl)

invisNode :: Diagram D.NodeId
invisNode = node [("style","invis"),("shape","point")]

reflow :: String -> String
-- FIXME: for now, you have to hardcode desired width/height ratio
-- FIXME: (tail $ init $ show) trick is needed to work around dotgen-0.2 limitations
reflow str = intercalate "\n" $ map unwords $ splitInto words_in_row w
      where w = words str
            z = length w
            rows = z*height `div` (height+width)
            words_in_row = rows*width `div` height
            chunk _ []  = Nothing
            chunk 0 lst = Just (lst, [])
            chunk n lst = Just $ splitAt n lst
            splitInto n = unfoldr (chunk n)
            width=3
            height=1

-- Return the ID of the next node in the swimline `name',
-- generating all required nodes and swimline connections along the way
connectToPrev :: String -> D.NodeId -> Diagram ()
connectToPrev "___tier" _ = return ()
connectToPrev sline currNode = do
  s <- getSwimline sline
  case s of
       -- Swimline already exists
       (Just prevNode) ->  do edge prevNode currNode [("style","dotted"),("arrowhead","none")]
                              setSwimline sline currNode
       -- Otherwise, swimline hase to be created
       (Nothing) -> do setSwimline sline currNode
                       -- Add heading node
                       heading <- node [("label", mkHeader sline),("shape","box")]
                       addHeading heading
                       setSwimline sline heading
                       -- Retry connecting
                       connectToPrev sline currNode

mkHeader :: String -> String
mkHeader = map remove_underscore
  where
    remove_underscore '_' = ' '
    remove_underscore x   = x

------------------------------
-- State access/modify helpers
------------------------------

incTier :: Diagram ()
incTier = modify (\e -> e {numTier = numTier e +1} )

getSwimline :: String -> Diagram (Maybe D.NodeId)
getSwimline name = do
  s <- gets swimlines
  return $ M.lookup name s

setSwimline :: String -> D.NodeId -> Diagram ()
setSwimline name x = do
  modify (\e -> e {swimlines = M.insert name x (swimlines e)})

addHeading :: D.NodeId -> Diagram ()
addHeading x = do
  modify (\e -> e {headings = x:(headings e)})

------------------------------------------------
-- Lifting Text.Dot functions to the State monad
------------------------------------------------
same = lift . D.same
node = lift . D.node
edge f t args = lift $ D.edge f t args

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

-- Parser tests
newtype Name = Name String
newtype Message = Message String

instance Arbitrary Name where
  arbitrary = liftM Name (listOf' $ elements "abcxyz_банк")

instance Arbitrary Message where
  -- words.unwords trick is needed to prevent Messages which contain only spaces
  arbitrary = liftM ((Message).unwords.words) $ frequency [ (50, listOf' $ elements "abcxyz_->; 123банк")
                                                          -- One special case which i decided to hard-code
                                                          , (1, return "foo -> bar")
                                                          ]

instance Arbitrary Flow where
  arbitrary = frequency [ (10, liftM3 Msg mkName mkName mkMsg)
                        , (5, liftM2 Action mkName mkMsg)
                        ]
    where
      mkName = do Name n <- arbitrary; return n
      mkMsg = do Message m <- arbitrary; return m

-- Taken from a unreleased version of quickcheck
-- Just added ' to the names
--   / Kolmodin
listOf' :: Gen a -> Gen [a]
listOf' gen = sized $ \n ->
  do k <- choose (1,n)
     vectorOf' k gen

vectorOf' :: Int -> Gen a -> Gen [a]
vectorOf' k gen = sequence [ gen | _ <- [1..k] ]


-- | Print element of the flow diagram as String
showFlow :: Flow -> String
showFlow (Order sl)   = "order " ++ intercalate " " sl
showFlow (Msg f t m)  = unwords [ f, " -> ", t, ":", m ]
showFlow (Action s a) = unwords [ s, ":", a ]

prop_reparse :: [Flow] -> Bool
prop_reparse x =
  let txt = unlines $ map showFlow x
      in x == parseFlow "" txt

prop_russian_k :: Bool
prop_russian_k =
  ( parseFlow "a->b" "A->B: клиент" == [Msg "A" "B" "клиент"] ) &&
  ( parseFlow "prod" "продавец -> клиент: подписание контракта, предоставление счета" == [Msg "продавец" "клиент" "подписание контракта, предоставление счета"] )
