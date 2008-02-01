{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Name        :  Dot
-- Copyright   :  (c) Dmitry Astapov, 2007
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Library for generatin Graphviz (www.graphviz.org) documents
-----------------------------------------------------------------------------
module Dot ( inSection
           , addString
           , addNode
           , addEdge
           , Style(..)
           , Shape(..)
           , Param(..)
           ,UsesDotEnv(..)
           ,DotEnv(..)
           ) where

import Control.Monad.State (gets, modify, State)
import qualified Data.Map as M
import Data.List (intersperse)

-- Since order of declaration matter for dot, we need to be able to generate nodes
-- "in the past". FIXME: document
type Section = String
type Contents = [String]
type Dotcument = M.Map Section Contents

data DotEnv = DotEnv { section::Section
                     -- ^ name of the current graph section
                     , dotcument :: Dotcument
                     -- ^ name of section => section contents
                     }

class Monad m => UsesDotEnv m where
  getDotcument :: m Dotcument
  setDotcument :: Dotcument -> m ()
  getSection  :: m Section
  setSection  :: Section -> m ()

instance UsesDotEnv (State DotEnv) where
  getDotcument   = gets dotcument
  setDotcument d = modify (\e -> e {dotcument = d})
  getSection     = gets section
  setSection s   = modify (\e -> e {section = s})

-- Dot language elements
data Param = Label String | Constraint Bool | Style Style
           | Shape Shape | ArrowHead String
data Style = Invis | Dotted | Filled
data Shape = Point | Plaintext

-- Generating functions
addString :: UsesDotEnv m => String -> m ()
addString = emit

-- addNodeDefaults :: UsesDotEnv m => [Param] -> m ()
-- addNodeDefaults params = addNode "node" params

addNode :: UsesDotEnv m => String -> [Param] -> m ()
addNode name params = emit $ unwords [name, mkParams params, ";"]

addEdge :: UsesDotEnv m => String -> String -> [Param] -> m ()
addEdge from_node to_node params =
  emit $ unwords $
         [ from_node
         , "->"
         , to_node
         , mkParams params
         , ";"
         ]

-- Graph generation helpers
mkParams :: [Param] -> String
mkParams [] = ""
mkParams p  = "[" ++ concat (intersperse "," (map show p)) ++ "]"

emit :: UsesDotEnv m => String -> m ()
emit s = do
  sec <- getSection
  d <- getDotcument
  setDotcument $ M.insertWith (++) sec [s] d

inSection :: UsesDotEnv m => String -> m a -> m ()
inSection name f = do
  sec <- getSection
  setSection name
  f
  setSection sec

-- Prettyprinters
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
