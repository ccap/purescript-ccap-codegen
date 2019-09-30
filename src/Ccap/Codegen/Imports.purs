module Ccap.Codegen.Imports
  ( Scope
  , ImportError
  , allImports
  , importParts
  , importModuleName
  , modulesWithImport
  , importInScope
  , checkImportsExist
  , parseImports
  , validateImports
  ) where

import Prelude

import Ccap.Codegen.FileSystem as FS
import Ccap.Codegen.Types (Import, Module, ModuleName)
import Ccap.Codegen.ValidationError (class ValidationError, toValidation)
import Control.Bind (bindFlipped)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Node.Path (FilePath)
import Node.Path as Path

type Scope =
  { source :: FilePath
  , included :: Array FilePath
  }

data ImportError
  = NotFound Module Import
  | ParseError Import String

instance importValidationError :: ValidationError ImportError where
  printError = case _ of
    NotFound mod imprt ->
      mod.name <> " tried to import module: " <> imprt
        <> " but it does not exist, or was not included."
    ParseError imprt message ->
      "Parsing imported module, " <> imprt <> ", failed with error: " <> message

importParts :: Import -> Array String
importParts = String.split (Pattern ".")

importModuleName :: Import -> ModuleName
importModuleName imprt = fromMaybe imprt $ Array.last $ importParts imprt

-- | All unique imports from parsed modules.
allImports :: Array Module -> Array Import
allImports = Array.nub <<< bindFlipped _.imports

-- | All modules that have import the same file.
modulesWithImport :: Import -> Array Module -> Array Module
modulesWithImport imprt = Array.filter (Array.elem imprt <<< _.imports)

-- | Return the actual file path of an imported file if it exists.
importInScope :: Scope -> Import -> Effect (Maybe FilePath)
importInScope { source, included } imprt =
  let
    importPath = String.replaceAll (Pattern ".") (Replacement Path.sep) imprt
    options = (source : included) <#> \d -> Path.concat [ d, importPath ] <> ".tmpl"
  in
    Array.filterA FS.isFile options <#> Array.head -- TODO: quit early (findM)

-- | Get the file paths for all imports or _an_ error message for _all_ imports that cannot be
-- | found. This is done over a batch of modules to avoid parsing the same file twice.
checkImportsExist :: Scope -> Array Module -> Effect (Either (Array ImportError) (Array FilePath))
checkImportsExist scope modules =
    traverse checkImportExists (allImports modules)
      <#> sequence
  where
    checkImportExists imprt =
      importInScope scope imprt
        <#> note (notFoundMsg imprt)
    notFoundMsg imprt =
      modulesWithImport imprt modules
        <#> flip NotFound imprt

-- | Really is just parsing an array of files right now but the Import type might get more
-- | complicated later.
parseImports :: Array FilePath -> Effect (Either (Array ImportError) (Array Module))
parseImports imports = traverse parse imports <#> toValidation
  where parse imprt = lmap (ParseError imprt) <$> FS.parseFile imprt

-- | Validate that the imports of the given modules exist and parse the imported modules
-- | Note: Does not validate the contents of the imported files.
validateImports :: Scope -> Array Module -> Effect (Either (Array ImportError) (Array Module))
validateImports scope modules = runExceptT do
  imports <- ExceptT $ checkImportsExist scope modules
  ExceptT $ parseImports imports
