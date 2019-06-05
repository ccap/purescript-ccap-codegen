module Ccap.Codegen.Shared
  ( Emit
  , Imports
  , OutputSpec
  , annotValue
  , emit
  , indented
  ) where

import Prelude

import Ccap.Codegen.Types (Annotation(..), AnnotationParam(..), Module)
import Control.Monad.Writer (Writer, tell)
import Data.Array as Array
import Data.Maybe (Maybe)
import Text.PrettyPrint.Boxes (Box, emptyBox, (<<>>))

type OutputSpec =
  { render :: Module -> String
  , fileName :: Module -> String
  }

indent :: Box
indent = emptyBox 0 2

indented :: Box -> Box
indented = (<<>>) indent

type Imports = Array String

type Emit = Writer Imports

emit :: forall a. Array String -> a -> Emit a
emit imports a = map (const a) (tell imports)

annotValue :: String -> String -> Array Annotation -> Maybe String
annotValue annotKey paramKey annots = do
  Annotation _ _ params <- Array.find (\(Annotation n _ _) -> n == annotKey) annots
  AnnotationParam _ _ v <- Array.find (\(AnnotationParam n _ _) -> n == paramKey) params
  v
