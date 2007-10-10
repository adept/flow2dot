module Main where

import System (getArgs)
import Control.Monad.State (State(..),evalState,gets,modify,when)
import qualified Data.Map as M
import Data.List (intersperse)
import Text.Regex.Posix ((=~))
import Data.Array (Array(),(!))
import Text.UTF8 (fromUTF8, toUTF8)

{-
Idea:
strict digraph SeqDiagram 
{
  { // Those are swimline heads
    rank=same
    actor [label="Some actor"];
    system [label="Some system"];
  }
  { //step1
    rank=same
    node[style=invis,shape=point];
    step1; // this is an "anchor" for 1st diagram tier
    actor1; // this is a 1st point on "actor" swimline
    system1; // this is a 1st point on "system" swimline
  }
  { //step2
    rank=same
    node[style=invis,shape=point];
    step2; // anchor for 2nd diagram tier
    actor2; // this is a 2nd point on "actor" swimline
    system2; // this is a 2nd point on "system" swimline
  }
  // Main body

  // Tiers ordering. We link "anchor" nodes and Dot will do the rest
  step1 -> step2;

  // Actual messages. Note the "constraint=false"
  actor1 -> system1[label="xxx", constraint=false]; 
  system2 -> actor2[label="yyy", constraint=false];
}
-}

main = do
  args <- getArgs
  case args of
       [fname] -> process fname
       _ -> do print "Usage: flow2dot file.flow > file.dot"


-- | Process a .flow file and output generated .dot diagram
process :: FilePath -> IO ()
process fname = do
  flow <- parseFlow fname
  putStrLn $ toUTF8 $ process' flow

-- FIXME: remove "zzzz_BODY" and rework section generation to emit body last
-- process' 
process' flow = evalState (flow2dot flow) (DiagS M.empty M.empty 1 "zzzz_BODY")

-- | .flow is parsed into Messages (from ---(message)---> to) 
-- and Preformatted strings which are passed to output .dot as-is
data Flow = Msg String String String 
          | Pre String deriving Show

-- | State of the diagram builder
data DiagS = DiagS { swimlines::M.Map String Int
                   -- ^ name, number of nodes
                   , step :: Int
                   -- ^ number of the next diagram step
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
  withSection "HEADING" $ addString "rank=same"
  mapM_ flowStep2dot flow
  d <- gets diagram
  return $ header ++ (concatMap genSection $ M.toList d) ++ footer
  where
    -- NB: "strict" is VERY important here
    -- Without it, "dot" segfaults while rendering diagram (dot 2.12)
    header = "strict digraph Seq {\n"
    footer = "}\n"
    genSection ("zzzz_BODY",contents) = unlines (reverse contents)
    genSection (name,contents) = "{ //" ++ name ++ "\n" ++ unlines (reverse contents) ++ "}\n"

flowStep2dot :: Flow -> Diagram ()
flowStep2dot (Pre l) = addString l
flowStep2dot (Msg from to message) = do
  s <- gets step
  let sec = "step" ++ show s
  -- Make a block where swimline nodes will be put. 
  -- Populate it with "step anchor" node
  withSection sec $ do addString "rank=same;"
                       addNodeDefaults [Style Invis, Shape Point]
                       addNode sec []
  -- Generate swimline nodes in this section
  f <- genNextNode sec from
  t <- genNextNode sec to
  -- Add swimline step into diagram body
  addEdge f t [ Label (show s ++ ": " ++ message)
              , Constraint False
              ]
  -- Connect step to previous
  when (s>1) $ do let sec' = "step" ++ show (s-1)
                  addEdge sec' sec [ Style Invis ]
  setStep (s+1)

-- Return the ID of the next node in the swimline `name',
-- generating all required nodes and swimline connections along the way
genNextNode sec name = do
  s <- gets swimlines
  case M.lookup name s of
       -- Swimline already exists
       (Just x) ->  do let x' = x+1
                       setSwimline name x'
                       -- Add new node
                       let next = name++(show x')
                       withSection sec $ addNode next []
                       -- Connect it to the rest of swimline
                       let prev = name++(show x)
                       addEdge prev next [Style Dotted, ArrowHead "none"]
                       return next
       -- Otherwise, swimline hase to be created
       (Nothing) -> do setSwimline name 1
                       -- Add heading
                       withSection "HEADING" $ addNode name [Label name]
                       -- Add first node
                       let first = name++"1"
                       withSection sec $ addNode first []
                       -- Connect it to the start of swimline
                       addEdge name first [Style Dotted, ArrowHead "none"]
                       return first

-- State helpers
setStep x = modify (\f -> f {step=x})
setSwimline name x = do 
  s <- gets swimlines
  modify (\f -> f {swimlines = M.insert name x s})
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
            (match:_) -> Msg (match!1) (match!1) (match!2)
    (match:_) -> Msg (match!1) (match!2) (match!3)
  
-- Graph generation
data Param = Label String | Constraint Bool | Style Style | Shape Shape | ArrowHead String
data Style = Invis | Dotted
data Shape = Point

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

withSection :: String -> Diagram a -> Diagram ()
withSection name f = do
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

instance Show Shape where
  show Point = "point"
