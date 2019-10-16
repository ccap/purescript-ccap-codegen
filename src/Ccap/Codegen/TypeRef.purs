module Ccap.Codegen.TypeRef
  ( TypeRefError
  , topTypeReferences
  , validateAllTypeRefs
  ) where

import Prelude

import Ccap.Codegen.Types (Import, Module, ModuleName, TRef, TopType(..), Type(..), TypeDecl, recordPropType, typeDeclTopType, typeDeclName)
import Ccap.Codegen.ValidationError (class ValidationError, toValidation)
import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (intercalate)
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)

type TypeName = String

type QTRef =
  { moduleName :: ModuleName
  , typeName :: TypeName
  }

data TypeRef
  = Qualified QTRef
  | Unqualified TypeName

data TypeRefError
  = QualifiedNotDefined Module QTRef
  | QualifiedNotImported Module QTRef
  | UnqualifiedNotDefined Module TypeName
  | UnqualifiedMultipleDefinitions Module TypeName (NonEmptyList Import)

instance typeRefValidationError :: ValidationError TypeRefError where
  printError = case _ of
    QualifiedNotDefined mod { moduleName, typeName } ->
      mod.name <> ": qualified reference of " <> typeName <> " is not defined in " <> moduleName
    QualifiedNotImported mod { moduleName, typeName } ->
      mod.name <> ": does not import module " <> moduleName
        <> " but uses it in qualified reference of " <> typeName
    UnqualifiedNotDefined mod typeName ->
      mod.name <> ": unqualified reference, " <> typeName <> ", is not defined."
    UnqualifiedMultipleDefinitions mod typeName imports ->
      mod.name <> ": unqualified reference, " <> typeName
      <> ", defined multiple times: " <> (intercalate ", " imports)

-- | Return all type references in a module
moduleTypeReferences :: Module -> Array TRef
moduleTypeReferences = _.types >=> typeDeclTopType >>> topTypeReferences

-- | Return all type references used in a Declared Type
topTypeReferences :: TopType -> Array TRef
topTypeReferences = case _ of
  Type typ -> typeReferences typ
  Wrap typ -> typeReferences typ
  Record props -> props >>= recordPropType >>> typeReferences
  Sum variants -> mempty

-- | Return all type references in any used type.
typeReferences :: Type -> Array TRef
typeReferences = case _ of
  Ref _ tRef -> pure tRef
  Primitive _ -> mempty
  Array typ -> typeReferences typ
  Option typ -> typeReferences typ

-- | Interpert a TypeRef as either a Qualified or Unqualified type reference.
fromTRef :: TRef -> TypeRef
fromTRef = case _ of
  { mod: Just moduleName, typ: typeName } -> Qualified { moduleName, typeName }
  { mod: Nothing, typ: typeName } -> Unqualified typeName

-- | Find a type declaration in a module.
findDeclaration :: TypeName -> Module -> Maybe TypeDecl
findDeclaration typeName = Array.find (eq typeName <<< typeDeclName) <<< _.types

-- | Find a module by name
findModule :: ModuleName -> Array Module -> Maybe Module
findModule moduleName = Array.find $ eq moduleName <<< _.name

-- | Validate all type references in a module by finding their type declarations in the module and
-- | it's imports.
validateAllTypeRefs :: Module -> Array Module -> Either (Array TypeRefError) (Array TypeDecl)
validateAllTypeRefs mod imports = moduleTypeReferences mod <#> fromTRef >>> validate # toValidation
  where validate typeRef = validateTypeRef typeRef mod imports

-- | Validate a type reference by finding its declaration.
validateTypeRef :: TypeRef -> Module -> Array Module -> Either TypeRefError TypeDecl
validateTypeRef = case _ of
  Qualified qtRef -> validateQtRef qtRef
  Unqualified typeName -> validateUnQRef typeName

-- | Validate a qualified reference against the imported modules.
validateQtRef :: QTRef -> Module -> Array Module -> Either TypeRefError TypeDecl
validateQtRef qtRef mod imports = do
  importedModule <-
    note (QualifiedNotImported mod qtRef) $
      findModule qtRef.moduleName imports
  note (QualifiedNotDefined mod qtRef) $
    findDeclaration qtRef.typeName importedModule

-- | Validate a unqualified reference against the current module and all imported modules.
validateUnQRef :: TypeName -> Module -> Array Module -> Either TypeRefError TypeDecl
validateUnQRef typeName mod imports =
  let
    findDecl imprt = findDeclaration typeName imprt <#> Tuple imprt
    matches = List.fromFoldable $ Array.catMaybes $ findDecl <$> (mod : imports)
  in
    case matches of
      Cons match maybeMore ->
        case NonEmptyList.fromList maybeMore of
          Nothing ->
            Right $ snd match
          Just moreMatches ->
            let multipleMatches = NonEmptyList.cons match moreMatches <#> fst >>> _.name
            in Left $ UnqualifiedMultipleDefinitions mod typeName multipleMatches
      Nil ->
        Left $ UnqualifiedNotDefined mod typeName
