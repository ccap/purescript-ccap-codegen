module Ccap.Codegen.Database
  ( domainModule
  , tableModule
  ) where

import Prelude
import Ccap.Codegen.TypeRef (topTypeReferences)
import Ccap.Codegen.Types (Annotation(..), AnnotationParam(..), Import, Module, Primitive(..), RecordProp, TopType(..), Type(..), TypeDecl(..), typeDeclName, typeDeclTopType)
import Control.Monad.Except (ExceptT, withExceptT)
import Data.Array as Array
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Monoid (guard)
import Database.PostgreSQL (Connection, PGError)
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

rowToDomain :: Row3 String String (Maybe Int) -> Domain
rowToDomain (Row3 domainName dataType charMaxLen) =
  { domainName
  , dataType
  , charMaxLen
  }

domainTypeDecl :: Domain -> TypeDecl
domainTypeDecl domain = TypeDecl domain.domainName (Wrap (dbType domain.dataType)) (annotations domain)

annotations :: forall r. { charMaxLen :: Maybe Int | r } -> Array Annotation
annotations = Array.fromFoldable <<< map maxLengthAnnotation <<< _.charMaxLen

maxLengthAnnotation :: Int -> Annotation
maxLengthAnnotation = Annotation "validations" emptyPos <<< Array.singleton <<< param
  where
  param = AnnotationParam "maxLength" emptyPos <<< Just <<< show

domainModule :: Pool -> String -> String -> ExceptT String Aff Module
domainModule pool scalaPkg pursPkg =
  withExceptT show
    $ withConnection pool \conn -> do
        results <- query conn (Query sql) Row0
        let
          types = Array.sortWith typeDeclName $ domainTypeDecl <<< rowToDomain <$> results
        pure
          $ { name: "Domains"
            , types
            , annots: []
            , imports: types >>= tableImports # Array.nub # Array.sort
            , exports:
              { scalaPkg
              , pursPkg
              , tmplPath: "Domains.tmpl"
              }
            }
  where
  sql =
    """
          select domain_name, data_type, character_maximum_length
          from information_schema.domains
          where domain_schema = 'public' and
                  data_type in ('numeric', 'character varying', 'character',
                                'integer', 'smallint', 'text', 'uuid',
                                'boolean', 'date', 'time without time zone',
                                'timestamp with time zone') and
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

tableModule :: Pool -> String -> String -> String -> ExceptT String Aff Module
tableModule pool scalaPkg pursPkg tableName =
  withExceptT show
    $ withConnection pool \conn -> do
        columns <- queryColumns tableName conn
        let
          decl = tableType tableName (columns <> [ occIdColumn ])
        pure
          { name: tableName
          , types: [ decl ]
          , annots: []
          , imports: tableImports decl # Array.sort
          , exports:
            { scalaPkg
            , pursPkg
            , tmplPath: tableName
            }
          }

tableImports :: TypeDecl -> Array Import
tableImports = typeDeclTopType >>> topTypeReferences >>> map _.mod >>> Array.catMaybes >>> Array.nub

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
                                'timestamp with time zone')
          order by ordinal_position ;
          """

tableType :: String -> Array DbColumn -> TypeDecl
tableType tableName columns = TypeDecl tableName (Record $ dbRecordProp <$> columns) []

dbRecordProp :: DbColumn -> RecordProp
dbRecordProp col@{ columnName, domainName, dataType, isNullable } =
  let
    baseType = maybe (dbType dataType) domainRef domainName

    optioned = if isNullable == "YES" then Option baseType else baseType

    annots = guard (isNothing domainName) annotations col
  in
    { name: columnName, typ: optioned, annots }

domainRef :: String -> Type
domainRef name = Ref emptyPos { mod: Just "Domains", typ: name }

dbType :: String -> Type
dbType dataType = case dataType of
  "numeric" -> Primitive PDecimal
  "character varying" -> Primitive PString
  "character" -> Primitive PString
  "integer" -> Primitive PInt
  "smallint" -> Primitive PInt
  "text" -> Primitive PString
  "boolean" -> Primitive PBoolean
  "date" -> Ref emptyPos { mod: Just "DateTimeSupport", typ: "Date" }
  "time without time zone" -> Ref emptyPos { mod: Just "DateTimeSupport", typ: "Time" }
  "timestamp with time zone" -> Ref emptyPos { mod: Just "DateTimeSupport", typ: "Timestamp" }
  "uuid" -> Ref emptyPos { mod: Just "UUIDSupport", typ: "UUID" }
  "occid" -> Ref emptyPos { mod: Just "OccSupport", typ: "OccId" }
  _ -> Primitive PString -- XXX
