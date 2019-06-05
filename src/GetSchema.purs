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
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Traversable (for_)
import Database.PostgreSQL.PG (newPool)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Generic (Foreign)
import Node.Yargs.Applicative (flag, rest, runY, yarg)
import Node.Yargs.Setup (usage)

app :: Boolean -> String -> Array Foreign -> Effect Unit
app domains tableParam fs = launchAff_ $ processResult do
  let checkString s =
        if String.length s > 0
          then Just s
          else Nothing
      table = checkString tableParam

  config <- except do
        pure { domains, table }

  fromDb <- dbModules config

  processModules config fromDb

dbModules :: Config -> ExceptT String Aff (Array Module)
dbModules config = do
  pool <- liftEffect $ newPool Database.poolConfiguration
  ds <-
    if config.domains
      then do
        d <- Database.domainModule pool
        pure [ d ]
      else pure []
  ts <-
    config.table # maybe
      (pure [])
      (\t -> do
        t' <- Database.tableModule pool t
        pure $ [ t' ])
  pure $ ds <> ts

type Config =
  { domains :: Boolean
  , table :: Maybe String
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
  let setup = usage "$0 --package <package> --mode <mode> a.tmpl"
  runY setup $ app <$> flag
                        "d"
                        [ "domains" ]
                        (Just "Query database domains")
                   <*> yarg
                        "t"
                        [ "table" ]
                        (Just "Query the provided database table")
                        (Left "")
                        true
                  <*> rest
