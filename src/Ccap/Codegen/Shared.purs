module Ccap.Codegen.Shared
  ( DelimitedLiteralDir(..)
  , Emit
  , ExtraImports
  , OutputSpec
  , delimitedLiteral
  , emit
  , indented
  ) where

import Prelude

import Ccap.Codegen.Types (Module)
import Control.Monad.Writer (class MonadTell, Writer, tell)
import Data.Array as Array
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hcat, vcat, (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (left, top) as Boxes

type OutputSpec =
  { render :: Module -> String
  , fileName :: Module -> String
  }

indent :: Box
indent = emptyBox 0 2

indented :: Box -> Box
indented = (<<>>) indent

type ExtraImports = Array String

type Emit = Writer (Array String)

emit :: forall m a. MonadTell ExtraImports m => Array String -> a -> m a
emit imports a = map (const a) (tell imports)

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
