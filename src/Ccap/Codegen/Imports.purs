module Ccap.Codegen.Imports
  ( validateImports

  ) where

import Prelude

import Ccap.Codegen.Types (Module(..), TypeDecl(..))
import Control.Monad.Except (ExceptT, except)
import Data.Array ((\\))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, maybe)
import Data.String as String
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff)
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (readdir, stat)
import Node.Path (FilePath)


findImports :: Array FilePath -> Effect (Array FilePath)
findImports paths = do
  hasDirectories <- (for paths \fp -> stat fp <#> isDirectory) <#> \lis -> Array.elem true lis
  if not hasDirectories
    then
      pure $ Array.filter
        (\fp -> String.contains (String.Pattern ".tmpl") fp)
        paths
    else do
      directories <-
        Array.filterA
          (\fp -> stat fp <#> isDirectory)
          paths
      append (paths \\ directories) <$>
        (findImports =<< Array.concat <$> for directories readdir)

validateImports :: Array String -> Array Module -> ExceptT String Aff Unit
validateImports fileNames modules =
  for_ modules \(Module name decls annots imports) -> do
    let fileFailIdx = Array.findIndex
          (eq false)
          (imports <#> \imprt -> Array.elem imprt fileNames)
        typeFailIdx = Array.findIndex
          (eq false)
          (decls <#> ensureTypeDeclExists)
        fileFailError idx =
          maybe
            (pure unit)
            (\i -> except $ Left
              $ name
              <> " tried to import module: "
              <> fromMaybe "" (Array.index fileNames i)
              <>  " but it does not exist, or was not added as input."
            )
            idx
        typeFailError idx =
          maybe
            (pure unit)
            (\i -> except $ Left
              $ name
              <> " tried to import type: "
              <> fromMaybe "" ((Array.index decls i) <#> \(TypeDecl tName _ _) -> tName)
              <> " but it is not included in any imports"
            )
            idx

    fileFailError fileFailIdx
    typeFailError typeFailIdx

  where
    ensureTypeDeclExists typeDecl =
      not $ Array.elem
        true
        (modules <#> \(Module _ decls _ _) -> Array.elem typeDecl decls)