{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Name        :  Flow2Dot
-- Copyright   :  (c) Dmitry Astapov, 2007
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Main where

import Dot

import System (getArgs)
import Control.Monad.State (State,evalState,gets,modify)
import qualified Data.Map as M
import Data.List (intersperse,unfoldr,splitAt)
import Text.UTF8 (fromUTF8, toUTF8)
import Data.Maybe (fromJust)
import Data.Char (isSpace)
import Test.QuickCheck
import Control.Monad (liftM, liftM2, liftM3)
import Text.ParserCombinators.Parsec hiding (State)

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
          | Pre String
            deriving (Eq,Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
       [fname] -> process fname
       _ -> do print "Usage: flow2dot file.flow > file.dot"


-- | Process a .flow file and output generated .dot diagram
process :: FilePath -> IO ()
process fname = do
  flow <- parseFlowFromFile fname
  putStrLn $ toUTF8 $ processFlow flow

-- FIXME: remove "zzzz_BODY" and rework section generation to emit body last
processFlow :: [Flow] -> String
processFlow flow = evalState (flow2dot flow) (DiagS M.empty 1 (DotEnv "zzzz_BODY" M.empty))

-- | State of the diagram builder
data DiagS = DiagS { swimlines::M.Map String Int
                   -- ^ name, number of nodes
                   , tier :: Int
                   -- ^ number of the next diagram tier
                   , dotEnv :: DotEnv
                   }

type Diagram = State DiagS

instance UsesDotEnv (State DiagS) where
  getDotcument   = gets (dotcument . dotEnv)
  setDotcument d = modify (\e -> let de = dotEnv e in e {dotEnv = de {dotcument = d}})
  getSection     = gets (section . dotEnv)
  setSection s   = modify (\e -> let de = dotEnv e in e {dotEnv = de {section = s}})


flow2dot :: [Flow] -> Diagram String
flow2dot flow = do
  inSection "HEADING" $ addString "rank=same"
  mapM_ flowElement2dot flow
  d <- getDotcument
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
  tir <- getTierName
  inSection tir $ do addString "rank=same;"
                     addNode tir [Style Invis, Shape Point]
  l <- mkLabel message
  genNextNode tir actor [Style Filled, Shape Plaintext, Label l]
  toNextTier

flowElement2dot (Msg from to message) = do
  tir <- getTierName
  inSection tir $ do addString "rank=same;"
                     addNode tir [Style Invis, Shape Point]
  f <- genNextNode tir from [Style Invis, Shape Point]
  t <- genNextNode tir to   [Style Invis, Shape Point]
  l <- mkLabel message
  addEdge f t [ Label l
              , Constraint False
              ]
  toNextTier

mkLabel :: String -> Diagram String
mkLabel lbl = do
  t <- gets tier
  return $ show t ++ ": " ++ reflow lbl
  where


reflow :: String -> String
-- FIXME: for now, you have to hardcode desired width/height ratio
reflow str = concat $ intersperse "\\n" $ map unwords $ splitInto words_in_row w
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

toNextTier :: Diagram ()
toNextTier = do
  tir <- getTierName
  prev <- getPrevTierName
  case prev of
       Nothing -> return ()
       Just p ->  addEdge p tir [ Style Invis ]
  incTier

-- Return the ID of the next node in the swimline `name',

-- generating all required nodes and swimline connections along the way
genNextNode :: String -> String -> [Param] -> Diagram String
genNextNode sec sline nodeparams = do
  s <- getSwimline sline
  case s of
       -- Swimline already exists
       (Just _) ->  do prev <- getSwimlineNodeName sline
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

mkHeader :: String -> String
mkHeader = map remove_underscore
  where
    remove_underscore '_' = ' '
    remove_underscore x   = x

-- State access/modify helpers
setTier :: Int -> Diagram ()
setTier x = modify (\f -> f {tier=x})

getTierName :: Diagram String
getTierName = do
  t <- gets tier
  return $ "tier" ++ show t

getPrevTierName :: Diagram (Maybe String)
getPrevTierName = do
  t <- gets tier
  if (t>1) then return $ Just $ "tier" ++ show (t-1)
           else return Nothing

incTier :: Diagram ()
incTier = modify (\e -> e {tier = tier e +1} )

getSwimline :: String -> Diagram (Maybe Int)
getSwimline name = do
  s <- gets swimlines
  return $ M.lookup name s

getSwimlineNodeName :: String -> Diagram String
getSwimlineNodeName name = do
  s <- getSwimline name
  return $ name ++ show (fromJust s)

setSwimline :: String -> Int -> Diagram ()
setSwimline name x = do
  modify (\e -> e {swimlines = M.insert name x (swimlines e)})

incSwimline :: String -> Diagram ()
incSwimline name = do
  s <- getSwimline name
  setSwimline name (fromJust s+1)


parseFlowFromFile :: FilePath -> IO [Flow]-- Parser
parseFlowFromFile fname = do
  raw <- readFile fname
  return $ parseFlow fname $ fst $ fromUTF8 raw

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
  return fl

flowLine, parseMsg, parseAction, parsePre :: GenParser Char st Flow
flowLine = try parseMsg <|> try parseAction <|> parsePre
parseMsg = do f <- identifier; string "->"; t <- identifier; string ":"; m <- anything
              return $ Msg f t (trim m)
parseAction = do s <- identifier; string ":"; a <- anything
                 return $ Action s (trim a)
parsePre = liftM Pre anything

identifier, whitespace, anything :: GenParser Char st String
identifier = do whitespace; i <- many (alphaNum <|> oneOf "_"); whitespace
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
  coarbitrary = undefined

instance Arbitrary Message where
  -- words.unwords trick is needed to prevent Messages which contain only spaces
  arbitrary = liftM ((Message).unwords.words) $ frequency [ (50, listOf' $ elements "abcxyz_->; 123банк")
                                                          -- One special case which i decided to hard-code
                                                          , (1, return "foo -> bar")
                                                          ]
  coarbitrary = undefined

instance Arbitrary Flow where
  arbitrary = frequency [ (10, liftM3 Msg mkName mkName mkMsg)
                        , (5, liftM2 Action mkName mkMsg)
                        , (2, liftM Pre mkMsg)
                        ]
    where
      mkName = do Name n <- arbitrary; return n
      mkMsg = do Message m <- arbitrary; return m
  coarbitrary = undefined

-- Taken from a unreleased version of quickcheck
-- Just added ' to the names
--   / Kolmodin
listOf' :: Gen a -> Gen [a]
listOf' gen = sized $ \n ->
  do k <- choose (1,n)
     vectorOf' k gen

vectorOf' :: Int -> Gen a -> Gen [a]
vectorOf' k gen = sequence [ gen | _ <- [1..k] ]


showFlow :: Flow -> String
showFlow (Msg f t m) = unwords [ f, " -> ", t, ":", m ]
showFlow (Action s a) = unwords [ s, ":", a ]
showFlow (Pre s) = s

prop_reparse :: [Flow] -> Bool
prop_reparse x =
  let txt = unlines $ map showFlow x
      in x == parseFlow "" txt

prop_russian_k :: Bool
prop_russian_k =
  ( parseFlow "a->b" "A->B: клиент" == [Msg "A" "B" "клиент"] ) &&
  ( parseFlow "prod" "продавец -> клиент: подписание контракта, предоставление счета" == [Msg "продавец" "клиент" "подписание контракта, предоставление счета"] )
