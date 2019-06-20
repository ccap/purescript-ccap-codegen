module Ccap.Codegen.Database
  ( domainModule
  , poolConfiguration
  , tableModule
  ) where

import Prelude

import Ccap.Codegen.Types (Annotation(..), AnnotationParam(..), Import(..), Imports, Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Control.Monad.Except (ExceptT, withExceptT)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL (Connection, PGError)
import Database.PostgreSQL.PG (Pool, PoolConfiguration, Query(..), defaultPoolConfiguration, query, withConnection)
import Database.PostgreSQL.Row (Row0(..), Row1(..), Row3(..), Row4(..))
import Effect.Aff (Aff)
import Text.Parsing.Parser.Pos (Position(..))

-- TODO should be configurable
poolConfiguration :: PoolConfiguration
poolConfiguration = (defaultPoolConfiguration "cc")
  { host = Just "dev15-db.wicourts.gov"
  , port = Just 5612
  , user = Just "viewer"
  , idleTimeoutMillis = Just 500
  }

emptyPos :: Position
emptyPos = Position { line: 0, column: 0 }

domainModule :: Pool -> ExceptT String Aff Module
domainModule pool = withExceptT show $ withConnection pool \conn -> do
  results <- query conn (Query sql) Row0
  let types = results <#> (\(Row3 domainName dataType (maxLen :: Maybe Int)) ->
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
  pure $ Module "Domains" [] types []
  where
    -- TODO Support other types (date/time types in particular)
    sql = """
          select domain_name, data_type, character_maximum_length
          from information_schema.domains
          where domain_schema = 'public' and
                  data_type in ('numeric', 'character varying', 'integer', 'smallint', 'text', 'boolean') and
                  domain_name <> 'BatchIDT'
          """

type DbColumn =
  { columnName :: String
  , dataType :: String
  , domainName :: Maybe String
  , isNullable :: String
  }

tableModule :: Pool -> String -> ExceptT String Aff Module
tableModule pool tableName = withExceptT show $ withConnection pool \conn -> do
  columns <- queryColumns tableName conn
  let Tuple decl imps = runWriter $ tableType tableName columns
  pure $ Module ("Db" <> tableName) (imps # Array.sort >>> Array.nub) [ decl ] []

queryColumns :: String -> Connection -> ExceptT PGError Aff (Array DbColumn)
queryColumns tableName conn = do
  results <- query conn (Query sql) (Row1 tableName)
  pure (results <#> \(Row4 columnName dataType domainName isNullable) ->
         { columnName, dataType, domainName, isNullable })
  where
    -- TODO Support other types (date/time types in particular)
    sql = """
          select column_name, data_type, domain_name, is_nullable
          from information_schema.columns
          where table_name = $1 and
                  data_type in ('numeric', 'character varying', 'integer', 'smallint', 'text', 'boolean')
          order by ordinal_position ;
          """

tableType :: String -> Array DbColumn -> Writer Imports TypeDecl
tableType tableName columns = do
  props <- for columns col
  pure $ TypeDecl ("Db" <> tableName) (Record props) []
  where
    col :: DbColumn -> Writer Imports RecordProp
    col { columnName, dataType, domainName, isNullable } = do
      baseType <- maybe (pure $ dbNameToType dataType) domain domainName
      let optioned =
            if isNullable == "YES"
              then Option baseType
              else baseType
      pure $ RecordProp columnName optioned

domain :: String -> Writer Imports Type
domain name = do
  tell [ Import "Domains" ]
  pure $ Ref emptyPos { mod: Just "Domains", typ: name }


dbNameToType :: String -> Type
dbNameToType = Primitive <<< case _ of
  "numeric" -> PDecimal
  "character varying" -> PString
  "integer" -> PInt
  "smallint" -> PInt
  "text" -> PString
  "boolean" -> PBoolean
  _ -> PString -- XXX
