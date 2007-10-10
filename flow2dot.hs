module Main where

import System (getArgs)
import Control.Monad.State (State(..),evalState,gets,modify,when)
import qualified Data.Map as M
import Data.List (intersperse,unfoldr,splitAt)
import Text.Regex.Posix ((=~))
import Data.Array (Array(),(!))
import Text.UTF8 (fromUTF8, toUTF8)
import Data.Maybe (fromJust)

{-
Idea: In order to draw sequence (flow) diagram using graphviz we can use directed layout (dot) to 
generate "skeleton" of the diagram and draw message lines and action boxes over it in "constraint=false" mode,
so that they would not disturb the "skeleton".

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

-- | Flow consists of:
-- 1)Messages: from ---(message)---> to
-- 2)Actions: "system" performs "action"
-- 3)Preformatted strings which are passed to output as-is
data Flow = Msg String String String 
          | Action String String
          | Pre String deriving Show

main = do
  args <- getArgs
  case args of
       [fname] -> process fname
       _ -> do print "Usage: flow2dot file.flow > file.dot"


-- | Process a .flow file and output generated .dot diagram
process :: FilePath -> IO ()
process fname = do
  flow <- parseFlow fname
  putStrLn $ toUTF8 $ processFlow flow

-- FIXME: remove "zzzz_BODY" and rework section generation to emit body last
processFlow flow = evalState (flow2dot flow) (DiagS M.empty 1 "zzzz_BODY" M.empty)

-- | State of the diagram builder
data DiagS = DiagS { swimlines::M.Map String Int
                   -- ^ name, number of nodes
                   , tier :: Int
                   -- ^ number of the next diagram tier
                   -- 
                   -- The following is really a second distinct State which I would like to separate out
                   -- 
                   , section::String
                   -- ^ name of the current graph section
                   , diagram::M.Map String [String]
                   -- ^ name of section, contents
                   }

type Diagram = State DiagS

flow2dot :: [Flow] -> Diagram String
flow2dot flow = do
  inSection "HEADING" $ addString "rank=same"
  mapM_ flowElement2dot flow
  d <- gets diagram
  return $ header ++ (concatMap genSection $ M.toList d) ++ footer
  where
    -- NB: "strict" is VERY important here
    -- Without it, "dot" segfaults while rendering diagram (dot 2.12)
    header = "strict digraph Seq {\n"
    footer = "}\n"
    genSection ("zzzz_BODY",contents) = unlines (reverse contents)
    genSection (name,contents) = "{ //" ++ name ++ "\n" ++ unlines (reverse contents) ++ "}\n"

flowElement2dot :: Flow -> Diagram ()
-- Pass preformatted lines to output as-is
flowElement2dot (Pre l) = addString l
-- Make a graph block where swimline nodes for the current tier will be put. 
-- Populate tier with "tier anchor" node
-- Generate nodes for message/action on all required swimlines
-- Connect generated nodes, if necessary
-- Connect tier to previous, which will ensure that tiers are ordered properly
flowElement2dot (Action actor message) = do
  tier <- getTierName
  inSection tier $ do addString "rank=same;"
                      addNode tier [Style Invis, Shape Point]
  l <- mkLabel message
  genNextNode tier actor [Style Filled, Shape Plaintext, Label l] 
  toNextTier
flowElement2dot (Msg from to message) = do
  tier <- getTierName
  inSection tier $ do addString "rank=same;"
                      addNode tier [Style Invis, Shape Point]
  f <- genNextNode tier from [Style Invis, Shape Point]
  t <- genNextNode tier to   [Style Invis, Shape Point]
  l <- mkLabel message
  addEdge f t [ Label l 
              , Constraint False
              ]
  toNextTier

mkLabel lbl = do
  t <- gets tier
  return $ show t ++ ": " ++ reflow lbl
  where

-- FIXME: for now, you have to hardcode desired width/height ratio
reflow str = concat $ intersperse "\\n" $ map unwords $ splitInto words_in_row w
      where w = words str
            z = length w
            rows = z*height `div` (height+width)
            words_in_row = rows*width `div` height
            chunk n []  = Nothing
            chunk 0 lst = Just (lst, [])
            chunk n lst = Just $ splitAt n lst
            splitInto n = unfoldr (chunk n)
            width=3
            height=1

toNextTier = do
  tier <- getTierName
  prev <- getPrevTierName
  case prev of
       Nothing -> return ()
       Just p ->  addEdge p tier [ Style Invis ]
  incTier


-- Return the ID of the next node in the swimline `name',
-- generating all required nodes and swimline connections along the way
genNextNode sec sline nodeparams = do
  s <- getSwimline sline
  case s of
       -- Swimline already exists
       (Just x) ->  do prev <- getSwimlineNodeName sline
                       incSwimline sline
                       next <- getSwimlineNodeName sline
                       -- Add new swimline node
                       inSection sec $ addNode next nodeparams
                       -- Connect it to the rest of swimline
                       addEdge prev next [Style Dotted, ArrowHead "none"]
                       return next
       -- Otherwise, swimline hase to be created
       (Nothing) -> do setSwimline sline 1
                       -- Add heading
                       inSection "HEADING" $ addNode sline [Label (mkHeader sline)]
                       -- Add first node
                       first <- getSwimlineNodeName sline
                       inSection sec $ addNode first nodeparams
                       -- Connect it to the start of swimline
                       addEdge sline first [Style Dotted, ArrowHead "none"]
                       return first

mkHeader = map remove_underscore
  where
    remove_underscore '_' = ' '
    remove_underscore x   = x

-- State access/modify helpers
setTier x = modify (\f -> f {tier=x})

getTierName = do
  t <- gets tier
  return $ "tier" ++ show t

getPrevTierName = do
  t <- gets tier
  if (t>1) then return $ Just $ "tier" ++ show (t-1)
           else return Nothing

incTier = do
  t <- gets tier
  setTier (t+1)

getSwimline name = do
  s <- gets swimlines
  return $ M.lookup name s

getSwimlineNodeName name = do
  s <- getSwimline name
  return $ name ++ show (fromJust s)

setSwimline name x = do 
  s <- gets swimlines
  modify (\f -> f {swimlines = M.insert name x s})

incSwimline name = do
  s <- getSwimline name
  setSwimline name (fromJust s+1)

setDiagram d = modify (\f -> f {diagram=d})

setSection s = modify (\f -> f {section=s})

-- Parser
parseFlow fname = do
  raw <- readFile fname
  return $ map parseLine $ lines $ fst $ fromUTF8 raw

parseLine "" = Pre ""
parseLine l  =
  case (l =~ "^\\s*(\\S*?)\\s*->\\s*(\\S*?):\\s*(.*?)\\s*$") :: [Array Int String] of
    [] -> case (l =~ "^\\s*(\\S*?)\\s*:\\s*(.*?)\\s*$") :: [Array Int String] of
            []      -> Pre l
            (match:_) -> Action (match!1) (match!2)
    (match:_) -> Msg (match!1) (match!2) (match!3)

----------------------------------------------------------------------
-- Graph generation
-- TODO: this could be expanded into standalone Text.Language.Graphviz
-- If only I could find a way to interveawe two State monads nicely
data Param = Label String | Constraint Bool | Style Style 
           | Shape Shape | ArrowHead String
data Style = Invis | Dotted | Filled
data Shape = Point | Plaintext

addString = emit

addNodeDefaults params = addNode "node" params

addNode :: String -> [Param] -> Diagram ()
addNode name params = emit $ unwords [name, mkParams params, ";"]

addEdge from_node to_node params =
  emit $ unwords $ 
         [ from_node
         , "->"
         , to_node
         , mkParams params
         , ";"
         ] 

-- Graph generation helpers
mkParams [] = ""
mkParams p  = "[" ++ concat (intersperse "," (map show p)) ++ "]"

emit s = do
  sec <- gets section
  d <- gets diagram
  setDiagram $ M.insertWith (++) sec [s] d

inSection :: String -> Diagram a -> Diagram ()
inSection name f = do
  sec <- gets section
  setSection name
  f
  setSection sec

instance Show Param where
  show (Label l) = "label=\""++l++"\""
  show (Style s) =  "style="++show s
  show (Constraint x) = "constraint=" ++ showB x
    where
      showB False = "false"
      showB True = "true"
  show (Shape x) = "shape=" ++ show x
  show (ArrowHead x) = "arrowhead="++x

instance Show Style where
  show Invis = "invis"
  show Dotted = "dotted"
  show Filled = "filled"

instance Show Shape where
  show Point = "point"
  show Plaintext = "plaintext"
