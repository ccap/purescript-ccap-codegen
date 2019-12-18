module Ccap.Codegen.Shared
  ( DelimitedLiteralDir(..)
  , OutputSpec
  , delimitedLiteral
  , indented
  , invalidate
  , modulesInScope
  ) where

import Prelude
import Ccap.Codegen.Types (Module, ValidatedModule)
import Data.Array ((:))
import Data.Array as Array
import Record (merge)
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hcat, vcat, (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (left, top) as Boxes

type OutputSpec
  = { render :: ValidatedModule -> String
    , filePath :: ValidatedModule -> String
    }

indent :: Box
indent = emptyBox 0 2

indented :: Box -> Box
indented = (<<>>) indent

invalidate :: ValidatedModule -> Module
invalidate vmod = merge { imports: (vmod.imports <#> _.name) } vmod

modulesInScope :: ValidatedModule -> Array Module
modulesInScope vmod = invalidate vmod : vmod.imports

data DelimitedLiteralDir
  = Vert
  | Horiz

delimitedLiteral ::
  DelimitedLiteralDir ->
  Char ->
  Char ->
  Array Box ->
  Box
delimitedLiteral dir l r boxes =
  let
    all =
      (Array.take 1 boxes <#> (char l <<+>> _))
        <> (Array.drop 1 boxes <#> (char ',' <<+>> _))
  in
    case dir of
      Vert -> vcat Boxes.left (all <> [ char r ])
      Horiz -> hcat Boxes.top (all <> [ char ' ' <<>> char r ])
