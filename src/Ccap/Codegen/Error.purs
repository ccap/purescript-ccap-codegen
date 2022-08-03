module Ccap.Codegen.Error
  ( Error(..)
  , Detail(..)
  , Import(..)
  , TypeDecl(..)
  , TypeRef(..)
  , toString
  ) where

import Prelude
import Ccap.Codegen.Cst as Cst
import Data.Maybe (maybe)
import Data.String as String
import Node.Path (FilePath)
import Parsing (Position(..))

data Error
  = Positioned FilePath Position Detail
  | FileRead FilePath String
  | ModuleNameMismatch FilePath { moduleName :: String, scalaName :: String, pursName :: String }

data Detail
  = Parse String
  | Import Import Cst.Import
  | TypeDecl TypeDecl Cst.TypeDecl
  | TypeRef TypeRef Cst.TRef

data Import
  = ImportNotFound
  | ImportCycle (Array FilePath)
  | MultipleMatches (Array FilePath)

data TypeDecl
  = DuplicateType
  | DuplicateRecordProperty Cst.RecordProp
  | DuplicateConstructorNames (Array Cst.ConstructorName)
  | UnknownTypeParam Cst.TypeParam
  | CircularType (Array String)

data TypeRef
  = QualifiedNotDefined
  | NotImported
  | TypeNotFound
  | IncorrectArity { found :: Int, expected :: Int }

toString :: Error -> String
toString = case _ of
  ModuleNameMismatch path { moduleName, scalaName, pursName } ->
    path
      <> ": the module name ("
      <> moduleName
      <> "), the Scala class name ("
      <> scalaName
      <> "), and the PureScript module name ("
      <> pursName
      <> ") must all match"
  FileRead path message -> path <> ": could not ready file: " <> message
  Positioned path (Position { line, column }) detail ->
    path <> ":" <> show line <> ":" <> show column <> ": "
      <> case detail of
          Parse msg -> msg
          Import importError (Cst.Import _ i) -> case importError of
            ImportNotFound -> "import " <> i <> " not found"
            ImportCycle paths -> "import " <> i <> " is part of a cycle:\n" <> String.joinWith "\n" (map ("    " <> _) paths)
            MultipleMatches paths -> "import " <> i <> " is defined in multiple templates:\n" <> String.joinWith "\n" (map ("    " <> _) paths)
          TypeDecl typeDeclError decl -> case typeDeclError of
            DuplicateType -> "type " <> Cst.typeDeclName decl <> " is defined multiple times"
            DuplicateRecordProperty { name } -> "record property " <> name <> " is defined multiple times"
            DuplicateConstructorNames names -> "constructor " <> String.joinWith ", " (map (\(Cst.ConstructorName name) -> name) names) <> " is defined multiple times"
            UnknownTypeParam (Cst.TypeParam param) -> "Unknown type parameter " <> param
            CircularType names ->
              "type "
                <> Cst.typeDeclName decl
                <> " is involved in a cycle:\n"
                <> String.joinWith "\n" (map ("    " <> _) names)
          TypeRef typeRefError { mod, typ } -> do
            let
              fullName = maybe typ (\(Cst.ModuleRef m) -> m <> "." <> typ) mod
            case typeRefError of
              TypeNotFound -> "type " <> typ <> " is not defined"
              QualifiedNotDefined -> "type " <> typ <> " is not defined in the imported template"
              NotImported -> fullName <> " does not reference an imported template"
              IncorrectArity { found, expected } -> fullName <> " was given " <> show found <> " type parameters, but should have been given " <> show expected
