module Ccap.Codegen.Database
  ( domains
  ) where

import Prelude

import Ccap.Codegen.Types (Module(..), Primitive(..), TopType(..), Type(..), TypeDecl(..))
import Control.Monad.Except (ExceptT, withExceptT)
import Database.PostgreSQL.PG (Pool, Query(..), query, withConnection)
import Database.PostgreSQL.Row (Row0(..), Row2(..))
import Effect.Aff (Aff)

domains :: Pool -> ExceptT String Aff Module
domains pool = withExceptT show $ withConnection pool \conn -> do
  results <- query conn (Query domainsSql) Row0
  let types = results <#> (\(Row2 domainName dataType) ->
                TypeDecl domainName (Type (Primitive (dbNameToPrimitive dataType))))
  pure $ Module "Domains" types

dbNameToPrimitive :: String -> Primitive
dbNameToPrimitive = case _ of
  "numeric" -> PDecimal
  "character varying" -> PString
  "integer" -> PInt
  "smallint" -> PInt
  "text" -> PString
  "boolean" -> PBoolean
  _ -> PString -- XXX

domainsSql :: String
domainsSql = """
  select domain_name, data_type
  from information_schema.domains
  where domain_schema = 'public' and
          data_type in ('numeric', 'character varying', 'integer', 'smallint', 'text', 'boolean')
  """
