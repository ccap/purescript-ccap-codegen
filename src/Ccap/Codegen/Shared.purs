module Ccap.Codegen.Shared
  ( Codegen
  , DelimitedLiteralDir(..)
  , Env
  , ExtraImports
  , OutputSpec
  , delimitedLiteral
  , emit
  , indented
  , runCodegen
  ) where

import Prelude

import Ccap.Codegen.Types (Module)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Writer (class MonadTell, Writer, runWriter, tell)
import Data.Array as Array
import Data.Tuple (Tuple)
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hcat, vcat, (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (left, top) as Boxes

type OutputSpec =
  { render :: Module -> String
  , filePath :: Module -> String
  }

indent :: Box
indent = emptyBox 0 2

indented :: Box -> Box
indented = (<<>>) indent

type ExtraImports = Array String

emit :: forall m a. MonadTell ExtraImports m => Array String -> a -> m a
emit imports a = map (const a) (tell imports)

type Env =
  { allModules :: Array Module
  , currentModule :: Module
  , defaultPrefix :: String
  }

type Codegen = ReaderT Env (Writer ExtraImports)

runCodegen :: forall a. Env -> Codegen a -> Tuple a ExtraImports
runCodegen env c = runWriter (runReaderT c env)

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
