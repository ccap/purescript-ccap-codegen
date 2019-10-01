module Ccap.Codegen.FileSystem
  ( mkDirP
  ) where

import Prelude

import Ccap.Codegen.Util (liftEffectSafely)
import Control.Monad.Except (ExceptT)
import Data.String as String
import Data.Traversable (for_, scanl)
import Effect (Effect)
import Effect.Aff (Aff)
import Node.FS.Sync as Sync
import Node.Path (FilePath)
import Node.Path (concat, normalize, sep) as Path

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
