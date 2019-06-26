module GetSchema
  ( main
  ) where

import Prelude

import Ccap.Codegen.Database as Database
import Ccap.Codegen.PrettyPrint as PrettyPrint
import Ccap.Codegen.Shared (OutputSpec)
import Ccap.Codegen.Types (Module)
import Ccap.Codegen.Util (liftEffectSafely, processResult, scrubEolSpaces)
import Control.Monad.Except (ExceptT, except)
import Data.Array (singleton) as Array
import Data.Either (Either(..), note)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Traversable (for_)
import Database.PostgreSQL.PG (PoolConfiguration, defaultPoolConfiguration, newPool)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Yargs.Applicative (flag, runY, yarg)
import Node.Yargs.Setup (usage)

app :: Boolean -> String -> String -> Effect Unit
app domains dbConfig tableParam = launchAff_ $ processResult do
  let checkString s =
        if String.length s > 0
          then Just s
          else Nothing
      table = checkString tableParam
  poolConfig <- except (readPoolConfig dbConfig)
  let config = { domains, table, poolConfig }
  fromDb <- dbModules config
  processModules config fromDb
  where
    readPoolConfig :: String -> Either String PoolConfiguration
    readPoolConfig s = fromParts parts
      where
        parts = String.split (String.Pattern ":") s
        poolConfig db = (defaultPoolConfiguration db) { idleTimeoutMillis = Just 500 }
        fromParts [ host, port, db, user ] = portFromString port <#> \p ->
          (poolConfig db)
            { host = Just host
            , port = Just p
            , user = Just user
            }
        fromParts [ host, port, db, user, password ] = portFromString port <#> \p->
          (poolConfig db)
            { host = Just host
            , port = Just p
            , user = Just user
            , password = Just password
            }
        fromParts _ =
          Left "Config parameter must be of the form <host>:<port>:<db>:<user>:<password> (password optional)"
        portFromString = note "Database port must be an integer" <<< Int.fromString

dbModules :: Config -> ExceptT String Aff (Array Module)
dbModules config = do
  pool <- liftEffect $ newPool config.poolConfig
  ds <-
    if config.domains
      then map Array.singleton (Database.domainModule pool)
      else pure []
  ts <-
    config.table # maybe
      (pure [])
      (map Array.singleton <<< Database.tableModule pool)
  pure $ ds <> ts

type Config =
  { domains :: Boolean
  , table :: Maybe String
  , poolConfig :: PoolConfiguration
  }

processModules :: Config -> Array Module -> ExceptT String Aff Unit
processModules config modules =
  writeOutput config modules PrettyPrint.outputSpec

writeOutput :: Config -> Array Module -> OutputSpec -> ExceptT String Aff Unit
writeOutput config modules outputSpec = liftEffectSafely do
  for_ modules
    (Console.info <<< scrubEolSpaces <<< outputSpec.render)

main :: Effect Unit
main = do
  let setup = usage "$0 --config <host>:<port>:<db>:<user> [ --domains | --table <table> ]"
  runY setup $ app <$> flag
                        "d"
                        [ "domains" ]
                        (Just "Query database domains")
                   <*> yarg
                        "c"
                        [ "config" ]
                        (Just "The database to use")
                        (Right "Config is required")
                        true
                   <*> yarg
                        "t"
                        [ "table" ]
                        (Just "Query the provided database table")
                        (Left "")
                        true
