module Ccap.Codegen.FileSystem
  ( isFile
  , joinPaths
  , mkDirP
  , parseFile
  , readTextFile
  ) where

import Prelude

import Ccap.Codegen.Parser (errorMessage, wholeFile)
import Ccap.Codegen.Types (Source, Module)
import Ccap.Codegen.Util (liftEffectSafely)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.String as String
import Data.Traversable (for_, scanl)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception as Error
import Node.Encoding (Encoding(..))
import Node.FS.Stats as Stat
import Node.FS.Sync as Sync
import Node.Path (FilePath)
import Node.Path (basenameWithoutExt, concat, normalize, sep) as Path
import Text.Parsing.Parser (runParser)

mkDirP :: FilePath -> ExceptT String Aff Unit
mkDirP dirPath =
  let
    targets = scanl joinPaths "" (splitFilePath dirPath)
  in
    for_ targets (liftEffectSafely <<< makeIfDoesNotExist)

makeIfDoesNotExist :: FilePath -> Effect Unit
makeIfDoesNotExist path = unlessM (Sync.exists path) (Sync.mkdir path)

splitFilePath :: FilePath -> Array FilePath
splitFilePath = String.split (String.Pattern Path.sep) <<< Path.normalize

joinPaths :: FilePath -> FilePath -> FilePath
joinPaths x y = Path.concat [ x, y ]

-- | If the given file path exists and is a file.
isFile :: FilePath -> Effect Boolean
isFile filePath =
  ifM (Sync.exists filePath)
    (Stat.isFile <$> Sync.stat filePath)
    (pure false)

-- | Read a text file and catch any errors messages
readTextFile :: FilePath -> Effect (Either String String)
readTextFile = map (lmap Error.message) <<< try <<< Sync.readTextFile UTF8

-- | Read and parse a tmpl file
parseFile :: FilePath -> Effect (Either String (Source Module))
parseFile filePath = runExceptT do
  contents <- ExceptT $ readTextFile filePath
  mod <- except $ lmap (errorMessage filePath) $ runParser contents wholeFile
  let moduleName = Path.basenameWithoutExt filePath ".tmpl" -- TODO: or "tmpl"
  pure { source: filePath, contents: mod { name = moduleName } }
