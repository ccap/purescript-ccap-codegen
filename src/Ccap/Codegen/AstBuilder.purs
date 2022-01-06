module Ccap.Codegen.AstBuilder
  ( build
  , parseAll
  ) where

import Prelude
import Ccap.Codegen.Ast as Ast
import Ccap.Codegen.Cst as Cst
import Ccap.Codegen.Error as Error
import Ccap.Codegen.FileSystem as FileSystem
import Ccap.Codegen.Parser as Parser
import Ccap.Codegen.Parser.Export as Export
import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Control.Monad.State (StateT)
import Control.Monad.State as S
import Control.Monad.Trans.Class as T
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Compactable (compact)
import Data.Either (Either(..), note)
import Data.Foldable (any, elem)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup as Validation
import Effect (Effect)
import Node.Path (FilePath)
import Node.Path as Path
import Text.Parsing.Parser.Pos (Position)

type BuildParams
  = { files :: Array FilePath
    , importPaths :: Array FilePath
    }

build :: BuildParams -> ExceptT (NonEmptyArray Error.Error) Effect (Array (Cst.Source Ast.Module))
build params@{ files } = do
  resolved <- T.lift (traverse (Path.resolve []) files)
  mods <- parseAll resolved
  let
    importResult = runExceptT (S.evalStateT (traverse (parseImports params) mods) { visiting: Set.empty, complete: Map.empty })
  modsWithImports <- ExceptT (map (lmap NonEmptyArray.singleton) importResult)
  except (S.evalStateT (traverse buildAstModule modsWithImports) Map.empty)

buildAstModule :: ModuleWithImports -> StateT (Map FilePath (Cst.Source Ast.Module)) (Either (NonEmptyArray Error.Error)) (Cst.Source Ast.Module)
buildAstModule (ModuleWithImports { mod, imports: is }) = do
  cache <- S.get
  case Map.lookup mod.source cache of
    Just m -> pure m
    Nothing -> do
      let
        Cst.ModuleName modName = mod.contents.name

        scalaName = fromMaybe mod.contents.exports.scalaPkg (Array.last (Export.split mod.contents.exports.scalaPkg))

        pursName = fromMaybe mod.contents.exports.pursPkg (Array.last (Export.split mod.contents.exports.pursPkg))
      if Array.length
        ( Array.nub
            [ modName
            , scalaName
            , pursName
            ]
        )
        /= 1 then
        T.lift (Left (NonEmptyArray.singleton (Error.ModuleNameMismatch mod.source { moduleName: modName, scalaName, pursName })))
      else
        pure unit
      importedModules <- for is \m@(ModuleWithImports { mod: i }) -> map _.contents (buildAstModule m)
      let
        allTypeDeclNames = map Cst.typeDeclName mod.contents.types

        params =
          { dups: findDups (map Cst.typeDeclName mod.contents.types)
          , filePath: mod.source
          , decls: Map.fromFoldable (map (\t -> Tuple (Cst.typeDeclName t) t) mod.contents.types)
          , importedModules: Map.fromFoldable (map (\m@(Ast.Module { name: Cst.ModuleName n }) -> Tuple n m) importedModules)
          , moduleName: mod.contents.name
          }
      decls <-
        T.lift
          ( Validation.toEither
              ( traverse (Validation.V <<< lmap NonEmptyArray.singleton <<< cstTypeDeclToAstTypeDecl params) mod.contents.types
              )
          )
      let
        result =
          { source: mod.source
          , contents:
              Ast.Module
                { types: decls
                , exports: mod.contents.exports
                , imports: importedModules
                , name: mod.contents.name
                }
          }
      S.modify_ (Map.insert mod.source result)
      pure result

type BuildTypeDeclParams
  = { dups :: Set String
    , filePath :: FilePath
    , decls :: Map String Cst.TypeDecl
    , importedModules :: Map String Ast.Module
    , moduleName :: Cst.ModuleName
    }

findDups :: NonEmptyArray String -> Set String
findDups ss =
  Set.fromFoldable
    ( compact
        ( map
            (\l -> if NonEmptyArray.length l > 1 then Just (NonEmptyArray.head l) else Nothing)
            (Array.group (Array.sort (NonEmptyArray.toArray ss)))
        )
    )

cstTypeDeclToAstTypeDecl :: BuildTypeDeclParams -> Cst.TypeDecl -> Either Error.Error Ast.TypeDecl
cstTypeDeclToAstTypeDecl buildParams@{ filePath, dups: typeDeclDups, importedModules, decls, moduleName } decl@(Cst.TypeDecl { position: pos, name, topType, annots, params: typeParams }) = do
  if Set.member name typeDeclDups then
    Left (Error.Positioned filePath pos (Error.TypeDecl Error.DuplicateType decl))
  else
    Right unit
  checkCycles Set.empty decl
  scalaDecoderType <- scalaDecoderTypeDecl decl
  case topType of
    Cst.Type typ -> map (\t -> Ast.TypeDecl { name, topType: Ast.Type t, annots, isPrimary: false, params: typeParams, scalaDecoderType }) (cstTypeToAstType pos typ)
    Cst.Wrap typ -> map (\t -> Ast.TypeDecl { name, topType: Ast.Wrap t, annots, isPrimary: false, params: typeParams, scalaDecoderType }) (cstTypeToAstType pos typ)
    Cst.Record props -> do
      let
        dups = findDups (map (\{ name: n } -> n) props)
      map
        (\ps -> Ast.TypeDecl { name, topType: Ast.Record ps, annots, isPrimary: Cst.ModuleName name == moduleName, params: typeParams, scalaDecoderType })
        (traverse (cstRecordPropToAstRecordProp dups) props)
    Cst.Sum constructors -> do
      let
        dups = findDups (map cstConstructorStringName constructors)
      if not Set.isEmpty dups then
        Left (Error.Positioned filePath pos (Error.TypeDecl (Error.DuplicateConstructorNames (map Cst.ConstructorName (Array.fromFoldable dups))) decl))
      else
        map
          (\cs -> Ast.TypeDecl { name, topType: Ast.Sum cs, annots, isPrimary: false, params: typeParams, scalaDecoderType })
          (traverse ((cstConstructorToAstConstructor pos) dups) constructors)
  where
  hasConstructorArguments :: NonEmptyArray Cst.Constructor -> Boolean
  hasConstructorArguments =
    any
      ( case _ of
          Cst.WithArgs _ _ -> true
          Cst.NoArg _ -> false
      )

  checkCycles :: Set String -> Cst.TypeDecl -> Either Error.Error Unit
  checkCycles visited (Cst.TypeDecl r) = do
    if Set.member r.name visited then
      Left (Error.Positioned filePath pos (Error.TypeDecl (Error.CircularType (Array.fromFoldable visited)) decl))
    else
      Right unit
    case r.topType of
      Cst.Type ty -> checkCyclesType ty
      Cst.Wrap ty -> Right unit
      Cst.Record props ->
        void
          ( for props \{ typ } -> case typ of
              Cst.TParam _ -> Right unit
              Cst.TType t -> checkCyclesType t
          )
      Cst.Sum _ -> Right unit
    where
    checkCyclesType :: Cst.Type -> Either Error.Error Unit
    checkCyclesType = case _ of
      Cst.Ref refPos ref@{ mod, params, typ } -> case mod of
        Nothing -> do
          d <- lookupCurrentModuleTypeDecl refPos ref
          checkCycles (Set.insert r.name visited) d
        Just n -> do
          void (lookupImportedModuleTypeDecl refPos ref n)
      Cst.Array (Cst.TType t) -> checkCyclesType t
      Cst.Array (Cst.TParam _) -> Right unit
      Cst.Option (Cst.TType t) -> checkCyclesType t
      Cst.Option (Cst.TParam _) -> Right unit
      Cst.Primitive _ -> Right unit
      Cst.TypeWithParens ty -> checkCyclesType ty

  scalaDecoderTypeDecl :: Cst.TypeDecl -> Either Error.Error (Maybe Ast.ScalaDecoderType)
  scalaDecoderTypeDecl (Cst.TypeDecl r) = do
    if not Array.null r.params then
      Right Nothing
    else case r.topType of
      Cst.Type ty -> scalaDecoderType ty
      Cst.Wrap ty -> scalaDecoderType ty
      Cst.Record _ -> Right (Just Ast.Form)
      Cst.Sum cs ->
        Right
          ( Just
              ( if hasConstructorArguments cs then
                  Ast.Form
                else
                  Ast.Field
              )
          )
    where
    scalaDecoderType :: Cst.Type -> Either Error.Error (Maybe Ast.ScalaDecoderType)
    scalaDecoderType = case _ of
      Cst.Ref refPos ref@{ mod } -> case mod of
        Nothing -> do
          d <- lookupCurrentModuleTypeDecl refPos ref
          scalaDecoderTypeDecl d
        Just n -> do
          Tuple _ (Ast.TypeDecl { scalaDecoderType: s }) <- lookupImportedModuleTypeDecl refPos ref n
          Right s
      Cst.Array (Cst.TType t) -> scalaDecoderType t
      Cst.Array (Cst.TParam _) -> Right Nothing
      Cst.Option (Cst.TType t) -> scalaDecoderType t
      Cst.Option (Cst.TParam _) -> Right Nothing
      Cst.Primitive _ -> Right (Just Ast.Field)
      Cst.TypeWithParens ty -> scalaDecoderType ty

  cstTypeToAstType :: Position -> Cst.Type -> Either Error.Error Ast.Type
  cstTypeToAstType typePos = case _ of
    Cst.Primitive p -> Right (Ast.Primitive p)
    Cst.TypeWithParens typ -> cstTypeToAstType typePos typ
    Cst.Array t -> map Ast.Array (cstTypeParamToAstTypeParam typePos t)
    Cst.Option t -> map Ast.Option (cstTypeParamToAstTypeParam typePos t)
    Cst.Ref refPos ref@{ mod, params, typ } -> case mod of
      Nothing -> do
        d@(Cst.TypeDecl { params: declParams }) <- lookupCurrentModuleTypeDecl refPos ref
        ps <- traverse (cstTypeParamToAstTypeParam typePos) params
        if Array.length params /= Array.length declParams then
          Left (Error.Positioned filePath refPos (Error.TypeRef (Error.IncorrectArity { found: Array.length params, expected: Array.length declParams }) ref))
        else
          Right
            ( Ast.Ref
                { decl: Nothing
                , typ
                , params: ps
                , isPrimaryRef: Cst.isRecord (Cst.typeDeclTopType d) && Cst.ModuleName typ == moduleName
                }
            )
      Just n -> do
        Tuple m d@(Ast.TypeDecl { topType: tt, isPrimary: isPrimaryRef, params: declParams, scalaDecoderType }) <- lookupImportedModuleTypeDecl refPos ref n
        ps <- traverse (cstTypeParamToAstTypeParam refPos) params
        if Array.length params /= Array.length declParams then
          Left (Error.Positioned filePath refPos (Error.TypeRef (Error.IncorrectArity { found: Array.length params, expected: Array.length declParams }) ref))
        else
          Right (Ast.Ref { decl: Just (Tuple m d), typ, params: ps, isPrimaryRef })

  lookupCurrentModuleTypeDecl :: Position -> Cst.TRef -> Either Error.Error Cst.TypeDecl
  lookupCurrentModuleTypeDecl refPos ref = do
    note
      (Error.Positioned filePath refPos (Error.TypeRef Error.TypeNotFound ref))
      (Map.lookup ref.typ decls)

  lookupImportedModuleTypeDecl :: Position -> Cst.TRef -> Cst.ModuleRef -> Either Error.Error (Tuple Ast.Module Ast.TypeDecl)
  lookupImportedModuleTypeDecl refPos ref (Cst.ModuleRef n) = do
    m@(Ast.Module { types }) <-
      note
        (Error.Positioned filePath refPos (Error.TypeRef Error.NotImported ref))
        (Map.lookup n importedModules)
    d@(Ast.TypeDecl _) <-
      note
        (Error.Positioned filePath refPos (Error.TypeRef Error.QualifiedNotDefined ref))
        (Array.find (\t -> Ast.typeDeclName t == ref.typ) types)
    pure (Tuple m d)

  cstRecordPropToAstRecordProp :: Set String -> Cst.RecordProp -> Either Error.Error Ast.RecordProp
  cstRecordPropToAstRecordProp dups prop@{ name: n, typ, annots: as, position } = do
    if Set.member n dups then
      Left (Error.Positioned filePath position (Error.TypeDecl (Error.DuplicateRecordProperty prop) decl))
    else
      Right unit
    map
      (\t -> { name: n, typ: t, annots: as })
      (cstTypeParamToAstTypeParam position typ)

  cstConstructorStringName :: Cst.Constructor -> String
  cstConstructorStringName = case _ of
    Cst.NoArg (Cst.ConstructorName c) -> c
    Cst.WithArgs (Cst.ConstructorName c) _ -> c

  cstConstructorToAstConstructor :: Position -> Set String -> Cst.Constructor -> Either Error.Error Ast.Constructor
  cstConstructorToAstConstructor conPos dups constructor = do
    case constructor of
      Cst.NoArg n -> Right (Ast.NoArg n)
      Cst.WithArgs n params -> map (Ast.WithArgs n) (traverse (cstTypeParamToAstTypeParam conPos) params)

  cstTypeParamToAstTypeParam :: Position -> Cst.TypeOrParam -> Either Error.Error Ast.TypeOrParam
  cstTypeParamToAstTypeParam p = case _ of
    Cst.TParam c ->
      if not elem c typeParams then
        Left (Error.Positioned filePath p (Error.TypeDecl (Error.UnknownTypeParam c) decl))
      else
        Right (Ast.TParam c)
    Cst.TType typ -> map Ast.TType (cstTypeToAstType p typ)

parseAll :: Array FilePath -> ExceptT (NonEmptyArray Error.Error) Effect (Array (Cst.Source Cst.Module))
parseAll files =
  ExceptT do
    result <- traverse (map ((lmap NonEmptyArray.singleton) <<< Validation.V) <<< parse) files
    pure (Validation.toEither (sequence result))

newtype ModuleWithImports
  = ModuleWithImports
  { mod :: Cst.Source Cst.Module
  , imports :: Array ModuleWithImports
  }

instance eqModuleWithImports :: Eq ModuleWithImports where
  eq (ModuleWithImports { mod: { source: source1 } }) (ModuleWithImports { mod: { source: source2 } }) = source1 == source2

instance ordModuleWithImports :: Ord ModuleWithImports where
  compare (ModuleWithImports { mod: { source: source1 } }) (ModuleWithImports { mod: { source: source2 } }) = compare source1 source2

derive instance genericModuleWithImports :: Generic ModuleWithImports _

instance showModuleWithImports :: Show ModuleWithImports where
  show t = genericShow t

type ImportState
  = { visiting :: Set FilePath
    , complete :: Map FilePath ModuleWithImports
    }

parseImports :: BuildParams -> Cst.Source Cst.Module -> StateT ImportState (ExceptT Error.Error Effect) ModuleWithImports
parseImports params mod = do
  S.modify_ (\ss -> ss { visiting = Set.insert mod.source ss.visiting })
  parsedImports <- traverse (parseImport params mod) mod.contents.imports
  let
    result = ModuleWithImports { mod, imports: parsedImports }
  S.modify_ (\ss -> ss { visiting = Set.delete mod.source ss.visiting, complete = Map.insert mod.source result ss.complete })
  pure result

parseImport :: BuildParams -> Cst.Source Cst.Module -> Cst.Import -> StateT ImportState (ExceptT Error.Error Effect) ModuleWithImports
parseImport params mod i@(Cst.Import position ref) = do
  s <- S.get
  path <- T.lift (ExceptT (imports params mod i))
  if Set.member path s.visiting then
    T.lift (except (Left (Error.Positioned mod.source position (Error.Import (Error.ImportCycle (Array.fromFoldable s.visiting)) i))))
  else case Map.lookup path s.complete of
    Just m -> pure m
    Nothing -> do
      parsed <- T.lift (ExceptT (parse path))
      parseImports params parsed

imports :: BuildParams -> Cst.Source Cst.Module -> Cst.Import -> Effect (Either Error.Error FilePath)
imports { importPaths } { source } i@(Cst.Import position ref) = do
  allPossible <- possibleImportPaths
  existing <- Array.filterA FileSystem.isFile allPossible
  pure case Array.nub existing of
    [] -> Left (Error.Positioned source position (Error.Import Error.ImportNotFound i))
    [ path ] -> Right path
    paths -> Left (Error.Positioned source position (Error.Import (Error.MultipleMatches paths) i))
  where
  possibleImportPaths :: Effect (Array FilePath)
  possibleImportPaths = traverse (Path.resolve [] <<< resolveImport) (Path.dirname source : importPaths)

  importPath :: String
  importPath = String.replaceAll (String.Pattern ".") (String.Replacement Path.sep) ref

  resolveImport :: FilePath -> FilePath
  resolveImport dirPath = FileSystem.joinPaths dirPath (importPath <> ".tmpl")

parseErrorToError :: Parser.Error -> Error.Error
parseErrorToError (Parser.Error filePath position message) = Error.Positioned filePath position (Error.Parse message)

parse :: FilePath -> Effect (Either Error.Error (Cst.Source Cst.Module))
parse filePath =
  runExceptT do
    contents <- ExceptT (map (lmap (Error.FileRead filePath)) (FileSystem.readTextFile filePath))
    except (lmap parseErrorToError (Parser.parseSource filePath contents))
