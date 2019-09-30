module Ccap.Codegen.Module
  ( validateModule
  , exportsForModule
  ) where

import Prelude

import Ccap.Codegen.Imports (Scope, validateImports)
import Ccap.Codegen.TypeRef (validateAllTypeRefs)
import Ccap.Codegen.Types (Module, ValidatedModule, Exports)
import Ccap.Codegen.ValidationError (class ValidationError, printError)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (for_)
import Effect (Effect)

-- | Validate imports and type references against the compile scope.
validateModule :: Scope -> Array Module -> Effect (Either (Array String) (Array ValidatedModule))
validateModule scope modules = runExceptT do
  imports <- withErrors $ ExceptT $ validateImports scope modules
  withErrors $ except $ for_ modules $ flip validateAllTypeRefs imports
  pure $ modules <#> \m -> m { imports = exportsForModule m imports }

withErrors
  :: forall f e a. Functor f
   => ValidationError e
   => ExceptT (Array e) f a
   -> ExceptT (Array String) f a
withErrors = withExceptT $ map printError

exportsForModule :: Module -> Array Module -> Array Exports
exportsForModule mod imports = Array.filter isImported imports <#> _.exports
  where isImported = flip Array.elem mod.imports <<< _.name
