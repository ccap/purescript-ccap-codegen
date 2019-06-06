module Ccap.Codegen.Shared
  ( Emit
  , Imports
  , OutputSpec
  , emit
  , indented
  ) where

import Prelude
import Ccap.Codegen.Types (Module)
import Control.Monad.Writer (Writer, tell)
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
