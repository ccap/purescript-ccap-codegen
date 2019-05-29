module Ccap.Codegen.Shared
  ( OutputSpec
  , indented
  ) where

import Ccap.Codegen.Types (Module)
import Text.PrettyPrint.Boxes (Box, emptyBox, (<<>>))

type OutputSpec =
  { render :: Module -> String
  , fileName :: Module -> String
  }

indent :: Box
indent = emptyBox 0 2

indented :: Box -> Box
indented = (<<>>) indent
