module Ccap.Codegen.Parser.Export where

import Data.Foldable (class Foldable, intercalate)
import Data.String (Replacement(..))
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Node.Path (FilePath)
import Node.Path as Path

sep :: String
sep = "."

split :: String -> Array String
split = String.split (Pattern sep)

join :: forall f. Foldable f => f String -> String
join = intercalate sep

toPath :: String -> FilePath
toPath = String.replaceAll (Pattern sep) (Replacement Path.sep)
