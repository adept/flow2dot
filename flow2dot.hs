-----------------------------------------------------------------------------
-- |
-- Name        :  Flow2Dot
-- Copyright   :  (c) Dmitry Astapov, 2007-2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>
-- Stability   :  beta
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Main where

import Text.FlowDiagram
import System.IO.UTF8 (putStrLn)
import Prelude hiding (putStrLn)
import System (getArgs)

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
  putStrLn $ flow2dot flow

