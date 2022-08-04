module Ccap.Codegen.Database
  ( domainModule
  , tableModule
  ) where

import Prelude
import Ccap.Codegen.Cst as Cst
import Control.Monad.Except (ExceptT, except, withExceptT)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (note)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Monoid (guard)
import Database.PostgreSQL (Connection, PGError(..))
import Database.PostgreSQL.PG (Pool, Query(..), query, withConnection)
import Database.PostgreSQL.Row (Row0(..), Row1(..), Row3(..), Row5(..))
import Effect.Aff (Aff)
import Text.Parsing.Parser.Pos (Position(..))

emptyPos :: Position
emptyPos = Position { line: 0, column: 0 }

type Domain
  = { domainName :: String
    , dataType :: String
    , charMaxLen :: Maybe Int
    }

type AliasedType
  = { name :: String
    , type :: Cst.Type
    }

aliasedTypes :: Array AliasedType
aliasedTypes = [ { name: "CaseNoT", type: Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "CaseNoSupport"), typ: "CaseNo", params: [] } } ]

rowToDomain :: Row3 String String (Maybe Int) -> Domain
rowToDomain (Row3 domainName dataType charMaxLen) =
  { domainName
  , dataType
  , charMaxLen
  }

domainTypeDecl :: Domain -> Cst.TypeDecl
domainTypeDecl domain = Cst.TypeDecl { position: emptyPos, name: domain.domainName, topType: Cst.Wrap (dbType domain.dataType), annots: annotations domain, params: [] }

annotations :: forall r. { charMaxLen :: Maybe Int | r } -> Array Cst.Annotation
annotations = Array.fromFoldable <<< map maxLengthAnnotation <<< _.charMaxLen

maxLengthAnnotation :: Int -> Cst.Annotation
maxLengthAnnotation = Cst.Annotation "validations" emptyPos <<< Array.singleton <<< param
  where
  param = Cst.AnnotationParam "maxLength" emptyPos <<< Just <<< show

domainModule :: Pool -> String -> String -> ExceptT String Aff Cst.Module
domainModule pool scalaPkg pursPkg =
  withExceptT show
    $ withConnection pool \conn -> do
        results <- query conn (Query sql) Row0
        let
          types = Array.sortWith Cst.typeDeclName $ domainTypeDecl <<< rowToDomain <$> results

          typesWithoutAlias =
            Array.filter
              ( \(Cst.TypeDecl typeDecl) ->
                  not (any (\aliasedType -> typeDecl.name == aliasedType.name) aliasedTypes)
              )
              types
        nelTypes <- except ((note (ConversionError "Expected at least one type")) (NonEmptyArray.fromArray typesWithoutAlias))
        pure
          { types: nelTypes
          , imports: types >>= tableImports # Array.nub # Array.sort
          , exports:
              { scalaPkg
              , pursPkg
              }
          , name: Cst.ModuleName "Domains"
          }
  where
  sql =
    """
          select distinct domain_name, data_type, character_maximum_length
          from information_schema.domains
          where domain_schema in ('public', 'dbtran') and
                  data_type in ('numeric', 'character varying', 'character',
                                'integer', 'smallint', 'text', 'uuid',
                                'boolean', 'date', 'time without time zone',
                                'timestamp with time zone', 'interval') and
                  domain_name not in ('BatchIDT', 'XMLT')
          """

type DbColumn
  = { columnName :: String
    , dataType :: String
    , domainName :: Maybe String
    , charMaxLen :: Maybe Int
    , isNullable :: String
    }

dbRowToColumn :: Row5 String String (Maybe String) (Maybe Int) String -> DbColumn
dbRowToColumn (Row5 columnName dataType domainName charMaxLen isNullable) =
  { columnName
  , dataType
  , domainName
  , charMaxLen
  , isNullable
  }

occIdColumn :: DbColumn
occIdColumn =
  { columnName: "occId"
  , dataType: "occid"
  , domainName: Nothing
  , charMaxLen: Nothing
  , isNullable: "NO"
  }

tableModule :: Pool -> String -> String -> String -> ExceptT String Aff Cst.Module
tableModule pool scalaPkg pursPkg tableName =
  withExceptT show
    $ withConnection pool \conn -> do
        columns <- queryColumns tableName conn
        nelColumns <- except (note (ConversionError ("Expected at least one column. Does the \"" <> tableName <> "\" table exist?")) (NonEmptyArray.fromArray columns))
        let
          decl = tableType tableName (nelColumns `NonEmptyArray.snoc` occIdColumn)
        pure
          { types: NonEmptyArray.singleton decl
          , imports: tableImports decl # Array.sort
          , exports:
              { scalaPkg
              , pursPkg
              }
          , name: Cst.ModuleName tableName
          }

tableImports :: Cst.TypeDecl -> Array Cst.Import
tableImports =
  Cst.typeDeclTopType
    >>> Cst.topTypeReferences
    >>> map _.mod
    >>> Array.catMaybes
    >>> Array.nub
    >>> map (\(Cst.ModuleRef name) -> Cst.Import emptyPos name)

queryColumns :: String -> Connection -> ExceptT PGError Aff (Array DbColumn)
queryColumns tableName conn = do
  results <- query conn (Query sql) (Row1 tableName)
  pure $ dbRowToColumn <$> results
  where
  sql =
    """
          select column_name, data_type, domain_name, character_maximum_length, is_nullable
          from information_schema.columns
          where table_name = $1 and
                  data_type in ('numeric', 'character varying', 'character',
                                'integer', 'smallint', 'text', 'uuid',
                                'boolean', 'date', 'time without time zone',
                                'timestamp with time zone', 'interval')
          order by ordinal_position ;
          """

tableType :: String -> NonEmptyArray DbColumn -> Cst.TypeDecl
tableType tableName columns = Cst.TypeDecl { position: emptyPos, name: tableName, topType: Cst.Record (dbRecordProp <$> columns), annots: [], params: [] }

dbRecordProp :: DbColumn -> Cst.RecordProp
dbRecordProp col@{ columnName, domainName, dataType, isNullable } =
  let
    baseType = maybe (dbType dataType) domainRef domainName

    optioned = if isNullable == "YES" then Cst.Option (Cst.TType baseType) else baseType

    annots = guard (isNothing domainName) annotations col
  in
    { name: columnName, typ: Cst.TType optioned, annots, position: emptyPos }

domainRef :: String -> Cst.Type
domainRef domainName = case Array.find (\aliasedType -> aliasedType.name == domainName) aliasedTypes of
  Just aliasedType -> aliasedType.type
  Nothing -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "Domains"), typ: domainName, params: [] }

dbType :: String -> Cst.Type
dbType dataType = case dataType of
  "numeric" -> Cst.Primitive Cst.PDecimal
  "character varying" -> Cst.Primitive Cst.PString
  "character" -> Cst.Primitive Cst.PString
  "integer" -> Cst.Primitive Cst.PInt
  "smallint" -> Cst.Primitive Cst.PInt
  "text" -> Cst.Primitive Cst.PString
  "boolean" -> Cst.Primitive Cst.PBoolean
  "date" -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "DateTimeSupport"), typ: "Date", params: [] }
  "time without time zone" -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "DateTimeSupport"), typ: "Time", params: [] }
  "timestamp with time zone" -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "DateTimeSupport"), typ: "Timestamp", params: [] }
  "uuid" -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "UUIDSupport"), typ: "UUID", params: [] }
  "interval" -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "DateTimeSupport"), typ: "Duration", params: [] }
  "occid" -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "OccSupport"), typ: "OccId", params: [] }
  _ -> Cst.Primitive Cst.PString -- XXX
