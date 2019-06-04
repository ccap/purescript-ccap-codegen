module GetSchema
  ( main
  ) where

import Prelude

import Ccap.Codegen.Database as Database
import Ccap.Codegen.PrettyPrint as PrettyPrint
import Ccap.Codegen.Shared (OutputSpec)
import Ccap.Codegen.Types (Module)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String as String
import Data.String.Regex (regex)
import Data.String.Regex (replace) as Regex
import Data.String.Regex.Flags (global, multiline) as Regex.Flags
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL.PG (newPool)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Error
import Foreign.Generic (Foreign)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as Sync
import Node.Process as Process
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

  for_ (fromDb) \(Tuple fileName modules) ->
    processModules config fileName modules

dbModules :: Config -> ExceptT String Aff (Array (Tuple String (Array Module)))
dbModules config =
  if config.domains || isJust config.table
    then do
      pool <- liftEffect $ newPool Database.poolConfiguration
      ds <-
        if config.domains
          then do
            d <- Database.domainModule pool
            pure $ [ Tuple "(domains query)" [ d ] ]
          else pure []
      ts <-
        config.table # maybe
          (pure [])
          (\t -> do
            t' <- Database.tableModule pool t
            pure $ [ Tuple ("(" <> t <> " table query)") [ t' ] ])
      pure $ ds <> ts
    else pure []

processResult :: ExceptT String Aff Unit -> Aff Unit
processResult r = do
  e <- runExceptT r
  e # either
    (\s -> liftEffect $ do
      Console.error $ "ERROR: " <> s
      Process.exit 1)
    pure

type Config =
  { domains :: Boolean
  , table :: Maybe String
  }

processModules :: Config -> String -> Array Module -> ExceptT String Aff Unit
processModules config fileName modules = do
  writeOutput config modules PrettyPrint.outputSpec

liftEffectSafely :: forall a. Effect a -> ExceptT String Aff a
liftEffectSafely = ExceptT <<< liftEffect <<< map (lmap Error.message) <<< try


writeOutput :: Config -> Array Module -> OutputSpec -> ExceptT String Aff Unit
writeOutput config modules outputSpec = liftEffectSafely do
  for_ modules
    (Console.info <<< scrubEolSpaces <<< outputSpec.render)
  where
    scrubEolSpaces :: String -> String
    scrubEolSpaces i =
      regex " +$" (Regex.Flags.multiline <> Regex.Flags.global) # either
        (const i)
        (\r -> Regex.replace r "" i)
    writeOutput_ :: Module -> Array String -> Effect Unit
    writeOutput_ mod dir =
      Sync.writeTextFile
        UTF8
        (String.joinWith "/" filePath)
        (scrubEolSpaces <<< outputSpec.render $ mod)
      where
        filePath = Array.snoc dir (outputSpec.fileName mod)

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
