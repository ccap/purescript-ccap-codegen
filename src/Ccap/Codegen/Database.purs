module Ccap.Codegen.Database
  ( domainModule
  , poolConfiguration
  ) where

import Prelude

import Ccap.Codegen.Types (Module(..), Primitive(..), TopType(..), Type(..), TypeDecl(..))
import Control.Monad.Except (ExceptT, withExceptT)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Database.PostgreSQL.PG (Pool, PoolConfiguration, Query(..), defaultPoolConfiguration, query, withConnection)
import Database.PostgreSQL.Row (Row0(..), Row2(..))
import Effect.Aff (Aff)

-- TODO should be configurable
poolConfiguration :: PoolConfiguration
poolConfiguration = (defaultPoolConfiguration "cc")
  { host = Just "dev15-db.wicourts.gov"
  , port = Just 5612
  , user = Just "viewer"
  , idleTimeoutMillis = Just 500
  }

domainModule :: Pool -> ExceptT String Aff Module
domainModule pool = withExceptT show $ withConnection pool \conn -> do
  results <- query conn (Query domainsSql) Row0
  let types = results <#> (\(Row2 domainName dataType) ->
                TypeDecl domainName (Wrap (Primitive (dbNameToPrimitive dataType)) Map.empty))
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
