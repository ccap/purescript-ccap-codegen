module Ccap.Codegen.Shared
  ( DelimitedLiteralDir(..)
  , Env
  , OutputSpec
  , delimitedLiteral
  , indented
  , invalidate
  ) where

import Prelude

import Ccap.Codegen.Types (Module, ValidatedModule)
import Data.Array as Array
import Data.Maybe (Maybe)
import Record (merge)
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hcat, vcat, (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (left, top) as Boxes

type OutputSpec =
  { render :: ValidatedModule -> String
  , filePath :: ValidatedModule -> String
  }

indent :: Box
indent = emptyBox 0 2

indented :: Box -> Box
indented = (<<>>) indent

type Env =
  { allModules :: Array Module
  , currentModule :: Module
  , defaultPrefix :: Maybe String
  }

invalidate :: ValidatedModule -> Module
invalidate vmod = merge { imports: (vmod.imports <#> _.name) } vmod

data DelimitedLiteralDir = Vert | Horiz

delimitedLiteral
  :: DelimitedLiteralDir
  -> Char
  -> Char
  -> Array Box
  -> Box
delimitedLiteral dir l r boxes =
  let all = (Array.take 1 boxes <#> (char l <<+>> _))
                  <> (Array.drop 1 boxes <#> (char ',' <<+>> _))
  in case dir of
       Vert -> vcat Boxes.left (all <> [ char r ])
       Horiz -> hcat Boxes.top (all <> [ char ' ' <<>> char r ])
