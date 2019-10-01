module Ccap.Codegen.Imports
  ( validateImports
  , findImports
  ) where

import Prelude

import Ccap.Codegen.Types (Module, TopType(..), Type(..), TypeDecl(..), ValidatedModule)
import Control.Monad.Except (ExceptT, except)
import Data.Array ((\\))
import Data.Array as Array
import Data.Either (Either(..))
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

-- | take a module and imported modules, and make sure the imports exist and the types exist.
validateImports :: Module -> Array Module -> ExceptT String Aff ValidatedModule
validateImports mod importedModules = do
  let fileFailIdx = Array.findIndex
        (eq false)
        (mod.imports <#> \imprt -> Array.elem imprt (importedModules <#> _.name)) -- ensure all module imports are in the imported modules
      typeFailIdx = Array.findIndex
        (eq false)
        (mod.types <#> ensureTypeDeclExists)
      fileFailError idx =
        maybe
          (pure unit)
          (\i -> except $ Left
            $ mod.name
            <> " tried to import module: "
            <> fromMaybe "" (Array.index (importedModules <#> _.name) i)
            <>  " but it does not exist, or was not added as input."
          )
          idx
      typeFailError idx =
        maybe
          (pure unit)
          (\i -> except $ Left
            $ mod.name
            <> " tried to import type: "
            <> fromMaybe "" ((Array.index mod.types i) <#> \(TypeDecl tName _ _) -> tName)
            <> " but it is not included in any imports"
          )
          idx

  fileFailError fileFailIdx
  typeFailError typeFailIdx
  pure $ mod
    { imports = (Array.filter (\impt -> Array.elem impt.name mod.imports) importedModules) <#> _.exports }

  where
    ensureTypeDeclExists typeDecl =
      not $ Array.elem
        true
        (mod.types <#> \t@(TypeDecl _ typ _) ->
          case typ of
            Type (Ref _ _) -> Array.elem true (Array.elem t <$> (importedModules <#> _.types))
            _ -> true
        )