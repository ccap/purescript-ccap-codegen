module Ccap.Codegen.Imports
  ( Includes
  , ImportError
  , importInScope
  , importsInScope
  , importsInScopes
  , parseImports
  , validateImports
  ) where

import Prelude

import Ccap.Codegen.FileSystem as FS
import Ccap.Codegen.Types (Import, Module, Source)
import Ccap.Codegen.ValidationError (class ValidationError, toValidation)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (bimap, rmap)
import Data.Either (Either, note)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Node.Path (FilePath)
import Node.Path as Path

type Includes = Array FilePath

data ImportError
  = NotFound Module Import
--  | MultipleFound Module Import
  | ParseError Import String

instance importValidationError :: ValidationError ImportError where
  printError = case _ of
    NotFound mod imprt ->
      mod.name <> " tried to import module: " <> imprt
        <> " but it does not exist, or was not included."
    ParseError imprt message ->
      "Parsing imported module, " <> imprt <> ", failed with error: " <> message

possibleImportPaths :: Includes -> FilePath -> Import -> Array FilePath
possibleImportPaths includes source imprt =
  let
    sourceDirs = Path.dirname source : includes
    importPath = String.replaceAll (Pattern ".") (Replacement Path.sep) imprt
  in
    sourceDirs <#> flip FS.joinPaths importPath <#> flip append ".tmpl"

importInScope :: Includes -> Source Module -> Import -> Effect (Either ImportError FilePath)
importInScope included { source, contents } imprt =
  let
    options = possibleImportPaths included source imprt
    existing = Array.filterA FS.isFile options
  in
    existing <#> Array.head >>> note (NotFound contents imprt) -- TODO error if multiple found

-- | Get the file paths for all imports or _an_ error message for _all_ imports that cannot be
-- | found. This is done over a batch of modules to avoid parsing the same file twice.
importsInScope
  :: Includes
  -> Source Module
  -> Effect (Either (Array ImportError) (Array FilePath))
importsInScope includes source =
  traverse (importInScope includes source) source.contents.imports
    <#> toValidation

importsInScopes
  :: Includes
  -> Array (Source Module)
  -> Effect (Either (Array ImportError) (Array FilePath))
importsInScopes includes sources =
  let
    validations :: Effect (Array (Either (Array ImportError) (Array FilePath)))
    validations = traverse (importsInScope includes) sources
  in validations <#> sequence <#> rmap (join >>> Array.nub)

-- | Really is just parsing an array of files right now but the Import type might get more
-- | complicated later.
parseImports :: Array FilePath -> Effect (Either (Array ImportError) (Array Module))
parseImports imports = traverse parse imports <#> toValidation
  where parse imprt = bimap (ParseError imprt) _.contents <$> FS.parseFile imprt

-- | Validate that the imports of the given modules exist and parse the imported modules
-- | Note: Does not validate the contents of the imported files.
validateImports
  :: Includes
  -> Array (Source Module)
  -> Effect (Either (Array ImportError) (Array Module))
validateImports includes sources = runExceptT do
  imports  <- ExceptT $ importsInScopes includes sources
  ExceptT $ parseImports imports
