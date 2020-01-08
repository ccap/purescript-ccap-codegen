module Ccap.Codegen.Module
  ( validateModules
  , importsForModule
  ) where

import Prelude
import Ccap.Codegen.Imports (Imported, Includes, possibleImportPaths, validateImports)
import Ccap.Codegen.TypeRef (validateAllTypeRefs)
import Ccap.Codegen.Types (Module, Source, ValidatedModule)
import Ccap.Codegen.ValidationError (printError)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (any)
import Data.Traversable (for)
import Effect (Effect)

-- | Validate imports and type references against the compile scope.
validateModules ::
  Includes ->
  Array (Source Module) ->
  Effect (Either (Array String) (Array (Source ValidatedModule)))
validateModules includes sources =
  runExceptT do
    allImports <- withExceptT (map printError) $ ExceptT $ validateImports includes sources
    except $ for sources $ validateModule includes allImports

validateModule ::
  Includes ->
  Array Imported ->
  Source Module ->
  Either (Array String) (Source ValidatedModule)
validateModule includes allImports source =
  let
    imports = importsForModule includes source allImports

    validatedSource = source { contents = source.contents { imports = imports } }

    validations = lmap (map printError) $ validateAllTypeRefs source.contents imports
  in
    validations *> pure validatedSource

importsForModule :: Includes -> Source Module -> Array Imported -> Array Module
importsForModule includes source =
  map _.contents.mod
    <<< Array.filter (isImportedBy includes source)

-- This was already done by validateImports, we should adjust the return type of that so we don't
-- have to do this again.
isImportedBy :: Includes -> Source Module -> Imported -> Boolean
isImportedBy includes source imported =
  let
    { source: modulePath, contents: { imports } } = source

    { source: importPath, contents: { imprt, mod: { name } } } = imported
  in -- TODO: this is almost identical to importInScope
    any (eq importPath) $ possibleImportPaths includes modulePath =<< imports
