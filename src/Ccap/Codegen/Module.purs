module Ccap.Codegen.Module
  ( validateModules
  , importsForModule
  ) where

import Prelude
import Ccap.Codegen.Imports (Imported, Includes, validateImports)
import Ccap.Codegen.TypeRef (validateAllTypeRefs)
import Ccap.Codegen.Types (Module, Source, ValidatedModule)
import Ccap.Codegen.ValidationError (class ValidationError, printError)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Control.MonadPlus (guard)
import Data.Either (Either)
import Data.Traversable (for)
import Effect (Effect)

-- | Validate imports and type references against the compile scope.
validateModules ::
  Includes ->
  Array (Source Module) ->
  Effect (Either (Array String) (Array (Source ValidatedModule)))
validateModules includes sources =
  runExceptT do
    allImports <- withErrors $ ExceptT $ validateImports includes sources
    withErrors $ except
      $ for sources \source -> do
          let
            mod = source.contents

            imports = importsForModule mod allImports
          _ <- validateAllTypeRefs mod imports
          pure $ source { contents = mod { imports = imports } }

withErrors ::
  forall f e a.
  Functor f =>
  ValidationError e =>
  ExceptT (Array e) f a ->
  ExceptT (Array String) f a
withErrors = withExceptT $ map printError

importsForModule :: Module -> Array Imported -> Array Module
importsForModule mod imports = do
  imported <- imports
  imprt <- mod.imports
  guard $ imprt == imported.imprt
  pure $ imported.mod
