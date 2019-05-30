module Ccap.Codegen.Database
  ( domainModule
  , poolConfiguration
  , tableModule
  ) where

import Prelude

import Ccap.Codegen.Types (Annotation(..), AnnotationParam(..), Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Control.Monad.Except (ExceptT, withExceptT)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..), maybe)
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
  let types = results <#> (\(Row3 domainName dataType maxLen_) ->
                let maxLen :: Maybe Int
                    maxLen = maxLen_
                    annots =
                      maybe
                        []
                        (\l ->
                          [ Annotation "validations" emptyPos
                              [ AnnotationParam "maxLength" emptyPos (Just $ show l)
                              ]
                          ])
                        maxLen
                in TypeDecl domainName (Wrap (Primitive (dbNameToPrimitive dataType)) Map.empty) annots)
  pure $ Module "Domains" types
  where
    -- TODO Support other types (date/time types in particular)
    sql = """
          select domain_name, data_type, character_maximum_length
          from information_schema.domains
          where domain_schema = 'public' and
                  data_type in ('numeric', 'character varying', 'integer', 'smallint', 'text', 'boolean')
          """

tableModule :: Pool -> String -> ExceptT String Aff Module
tableModule pool tableName = withExceptT show $ withConnection pool \conn -> do
  results <- query conn (Query sql) (Row1 tableName)
  let props = results <#> (\(Row4 columnName dataType domainName isNullable) ->
                              mkRecordProp columnName dataType domainName isNullable)
  pure $ Module ("Db" <> tableName) [ TypeDecl ("Db" <> tableName) (Record props) [] ]
  where
    mkRecordProp :: String -> String -> Maybe String -> String -> RecordProp
    mkRecordProp columnName dataType domainName isNullable =
      let baseType = maybe
                      (Primitive (dbNameToPrimitive dataType))
                      -- TODO Need proper external reference
                      (Ref emptyPos <<< ("Domains." <> _))
                      domainName
          optioned =
            if isNullable == "YES"
              then Option baseType
              else baseType
      in RecordProp columnName optioned
    -- TODO Support other types (date/time types in particular)
    sql = """
          select column_name, data_type, domain_name, is_nullable
          from information_schema.columns
          where table_name = $1 and
                  data_type in ('numeric', 'character varying', 'integer', 'smallint', 'text', 'boolean')
          order by ordinal_position ;
          """

dbNameToPrimitive :: String -> Primitive
dbNameToPrimitive = case _ of
  "numeric" -> PDecimal
  "character varying" -> PString
  "integer" -> PInt
  "smallint" -> PInt
  "text" -> PString
  "boolean" -> PBoolean
  _ -> PString -- XXX
