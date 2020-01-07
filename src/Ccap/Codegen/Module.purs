module Ccap.Codegen.Module
  ( validateModules
  , importsForModule
  ) where

import Prelude
import Ccap.Codegen.Imports (Imported, Includes, resolveImport, validateImports)
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
import Node.Path as Path

-- | Validate imports and type references against the compile scope.
validateModules ::
  Includes ->
  Array (Source Module) ->
  Effect (Either (Array String) (Array (Source ValidatedModule)))
validateModules includes sources =
  runExceptT do
    allImports <- withExceptT (map printError) $ ExceptT $ validateImports includes sources
    except $ for sources $ validateModule allImports

validateModule :: Array Imported -> Source Module -> Either (Array String) (Source ValidatedModule)
validateModule allImports source =
  let
    imports = importsForModule source allImports

    validatedSource = source { contents = source.contents { imports = imports } }

    validations = lmap (map printError) $ validateAllTypeRefs source.contents imports
  in
    validations *> pure validatedSource

importsForModule :: Source Module -> Array Imported -> Array Module
importsForModule source = map _.contents.mod <<< Array.filter (isImportedBy source)

-- This was already done by validateImports, we should adjust the return type of that so we don't
-- have to do this again.
isImportedBy :: Source Module -> Imported -> Boolean
isImportedBy source imported =
  let
    { source: modulePath, contents: { imports } } = source

    { source: importPath, contents: { imprt, mod: { name } } } = imported
  in
    any (eq importPath) $ resolveImport (Path.dirname modulePath) <$> imports
