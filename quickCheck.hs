#!/usr/bin/runghc
{-

 Copyright (c) 2006 Dmitry Astapov <dastapov@gmail.com>
 Copyright (c) 2005 Lennart Kolmodin <kolmodin@dtek.chalmers.se>

 Permission to use, copy, modify, and distribute this software for any
 purpose with or without fee is hereby granted, provided that the above
 copyright notice and this permission notice appear in all copies.

 THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-}

{-
Inspired by similar script on John Hughes homepage
http://www.cs.chalmers.se/~rjmh/QuickCheck/quickcheck

This program looks through the current directory and searches for
haskell source files (.hs and .lhs).
In these files it looks for QuickCheck properties and
invokes ghci on them.

It's not tested that well, patches are welcome. :)
-}


module Main where

import Control.Monad

import Data.List

import System.Cmd
import System.Environment (getArgs)
import System.Exit
import System.Directory
import System.Posix.Files

-- options passed to ghci
opts = [ "-fglasgow-exts"  -- enable glasgow extensions
       , "-v0"             -- make ghci less verbose
       , "-w"              -- ignore all warnings
       ]

-- put the ghci of your choice here
ghci = "ghci"

test_opts = "TestOptions { no_of_tests = 100, length_of_tests = 5, debug_tests = False}"

main :: IO ()
main = do
  args <- getArgs
  let root = if null args then "." else head args
  files <- walkFiles root
  let hs_files = filter (not . exclude) files
  script <- liftM (unlines.concat) $ mapM prepare hs_files
  process opts script


-- | Determine if we should exclude the file from QuickCheck testing.
-- It has to be a haskell file (.hs or .lhs), and not start with "_darcs".
-- TODO: would it be more natural to say which files we should include instead?
exclude :: FilePath -> Bool
exclude fs =
    "_darcs" `isPrefixOf` fs || (not $ any (`isSuffixOf` fs) [".hs", ".lhs"])


-- | Return all files in a directory and its subdirectories.
-- Does not follow links or anything fancy like that.
walkFiles :: FilePath -> IO [FilePath]
walkFiles fp = do
  paths <- getDirectoryContents fp `catch` \_ -> return []
  let paths' = filter (`notElem` [".", ".."]) paths
  let loop files dirs [] = return (files, dirs)
      loop files dirs (x:xs) = do
        fs <- getFileStatus x
	case () of
	  _ | isRegularFile fs -> loop (x:files) dirs xs
	  _ | isDirectory fs   -> loop files (x:dirs) xs
	  _ | otherwise        -> loop files dirs xs -- ignore all other
  (files, dirs) <- loop [] [] $ map (fp </>) paths'
  rec <- concatMapM walkFiles dirs
  return (files ++ rec)
  where
  concatMapM f xs = liftM concat $ mapM f xs
  -- quick'n'dirty implementation of </>
  -- it joins two paths
  (</>) ""  b = b
  (</>) "." b = b
  (</>) a   b = a ++ '/':b


-- | Returns a list of properties defined in a file.
-- Ignore files it could not read and prints a warning.
-- TODO: We could use more fancy stuff for this. The current way is
--       a bit barbaric.
findProperties :: FilePath -> IO [String]
findProperties file_name = do
  raw_lines <- liftM lines (readFile file_name)
		`catch` \_ -> do
		putStrLn $ " !!! Could not read " ++ file_name
		return []
  let hs_lines | ".lhs" `isSuffixOf` file_name = map unlit raw_lines
               | otherwise		       = raw_lines
      props = nub $ filter ("prop_" `isPrefixOf`) $
  			map (fst . head . lex) hs_lines
  return props 
  where
  unlit ('>':' ':xs) = xs
  unlit xs           = xs

-- | Prepare a script for ghci which will run
-- all properties in given file
prepare :: FilePath -> IO [String]
prepare file_name = do
  props <- findProperties file_name
  return $ if (not . null $ props) 
           then [ ":l " ++ file_name
                , ":m + Test.QuickCheck Test.QuickCheck.Batch"
                , concat [ "runTests \""
                         , file_name 
                         , "\" (" ++ test_opts ++ ") [ " 
                         , concat (intersperse ", " $ map ("run "++) props) 
                         , " ]"
                         ]
                ]
           else []

-- | Run all the properties in a file through ghci.
-- TODO: What if we don't have write permission in the current directory?
--       It's better to create a proper temporary file than a fix one.
process :: [String] -> String -> IO ()
process options script = do
  writeFile "qc_in" script
  ec <- system (concat $ intersperse " " $ ghci : options ++ ["<qc_in"])
  case ec of
    ExitFailure i -> putStrLn $ "ghci exited with error " ++ show i
    _             -> return ()
  removeFile "qc_in"
  return ()

