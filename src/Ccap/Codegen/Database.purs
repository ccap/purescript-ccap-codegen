module Ccap.Codegen.Database
  ( domainModule
  , tableModule
  ) where

import Prelude

import Ccap.Codegen.TypeRef (topTypeReferences)
import Ccap.Codegen.Types (Annotation(..), AnnotationParam(..), Import, Module, Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..), typeDeclName, typeDeclTopType)
import Control.Monad.Except (ExceptT, withExceptT)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Database.PostgreSQL (Connection, PGError)
import Database.PostgreSQL.PG (Pool, Query(..), query, withConnection)
import Database.PostgreSQL.Row (Row0(..), Row1(..), Row3(..), Row4(..))
import Effect.Aff (Aff)
import Text.Parsing.Parser.Pos (Position(..))

emptyPos :: Position
emptyPos = Position { line: 0, column: 0 }

domainModule :: Pool -> String -> String -> ExceptT String Aff Module
domainModule pool scalaPkg pursPkg = withExceptT show $ withConnection pool \conn -> do
  results <- query conn (Query sql) Row0
  let
    types =
      Array.sortWith typeDeclName $ results <#> (\(Row3 domainName dataType (maxLen :: Maybe Int)) ->
        let annots =
              maybe
                []
                (\l ->
                  [ Annotation "validations" emptyPos
                      [ AnnotationParam "maxLength" emptyPos (Just $ show l)
                      ]
                  ])
                maxLen
        in TypeDecl domainName (Wrap (dbNameToType dataType)) annots)
  pure $
    { name: "Domains"
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
    sql = """
          select domain_name, data_type, character_maximum_length
          from information_schema.domains
          where domain_schema = 'public' and
                  data_type in ('numeric', 'character varying',
                                'integer', 'smallint', 'text', 'uuid',
                                'boolean', 'date', 'time without time zone',
                                'timestamp with time zone') and
                  domain_name not in ('BatchIDT', 'XMLT')
          """

type DbColumn =
  { columnName :: String
  , dataType :: String
  , domainName :: Maybe String
  , isNullable :: String
  }

tableModule :: Pool -> String -> String -> String -> ExceptT String Aff Module
tableModule pool scalaPkg pursPkg tableName = withExceptT show $ withConnection pool \conn -> do
  columns <- queryColumns tableName conn
  let decl = tableType tableName columns
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
  pure (results <#> \(Row4 columnName dataType domainName isNullable) ->
         { columnName, dataType, domainName, isNullable })
  where
    sql = """
          select column_name, data_type, domain_name, is_nullable
          from information_schema.columns
          where table_name = $1 and
                  data_type in ('numeric', 'character varying',
                                'integer', 'smallint', 'text', 'uuid',
                                'boolean', 'date', 'time without time zone',
                                'timestamp with time zone')
          order by ordinal_position ;
          """

tableType :: String -> Array DbColumn -> TypeDecl
tableType tableName columns =
  TypeDecl tableName (Record (map col columns)) []
  where
    col :: DbColumn -> RecordProp
    col { columnName, dataType, domainName, isNullable } = do
      let baseType = maybe (dbNameToType dataType) domain domainName
          optioned =
            if isNullable == "YES"
              then Option baseType
              else baseType
      RecordProp columnName optioned

domain :: String -> Type
domain name =
  Ref emptyPos { mod: Just "Domains", typ: name }

dbNameToType :: String -> Type
dbNameToType =
  case _ of
    "numeric" -> Primitive PDecimal
    "character varying" -> Primitive PString
    "integer" -> Primitive PInt
    "smallint" -> Primitive PInt
    "text" -> Primitive PString
    "boolean" -> Primitive PBoolean
    "date" -> Ref emptyPos { mod: Just "DateTimeSupport", typ: "Date" }
    "time without time zone" -> Ref emptyPos { mod: Just "DateTimeSupport", typ: "Time" }
    "timestamp with time zone" -> Ref emptyPos { mod: Just "DateTimeSupport", typ: "Timestamp" }
    "uuid" -> Ref emptyPos { mod: Just "UUIDSupport", typ: "UUID" }
    _ -> Primitive PString -- XXX
