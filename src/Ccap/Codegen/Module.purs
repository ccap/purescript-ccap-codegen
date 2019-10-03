module Ccap.Codegen.Module
  ( validateModules
  , exportsForModule
  ) where

import Prelude

import Ccap.Codegen.Imports (Includes, validateImports)
import Ccap.Codegen.TypeRef (validateAllTypeRefs)
import Ccap.Codegen.Types (Exports, Module, ValidatedModule, Source)
import Ccap.Codegen.ValidationError (class ValidationError, printError)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (for_)
import Effect (Effect)

-- | Validate imports and type references against the compile scope.
validateModules
  :: Includes
  -> Array (Source Module)
  -> Effect (Either (Array String) (Array (Source ValidatedModule)))
validateModules includes sources = runExceptT do
  imports <- withErrors $ ExceptT $ validateImports includes sources
  let modules = sources <#> _.contents
  withErrors $ except $ for_ modules $ flip validateAllTypeRefs imports
  pure $ sources <#> \source -> source
    { contents = source.contents
      { imports = exportsForModule source.contents imports
      }
    }

withErrors
  :: forall f e a. Functor f
   => ValidationError e
   => ExceptT (Array e) f a
   -> ExceptT (Array String) f a
withErrors = withExceptT $ map printError

exportsForModule :: Module -> Array Module -> Array Exports
exportsForModule mod imports = Array.filter isImported imports <#> _.exports
  where isImported = flip Array.elem mod.imports <<< _.name
