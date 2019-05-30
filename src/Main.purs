module Main
  ( main
  ) where

import Prelude

import Ccap.Codegen.Database (domainModule, poolConfiguration) as Database
import Ccap.Codegen.Parser (errorMessage, roundTrip, wholeFile)
import Ccap.Codegen.PrettyPrint (prettyPrint) as PrintPrinter
import Ccap.Codegen.Purescript (prettyPrint) as Purescript
import Ccap.Codegen.Scala (prettyPrint) as Scala
import Ccap.Codegen.Types (Module)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT, withExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Database.PostgreSQL.PG (newPool)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, info) as Console
import Effect.Exception (message) as Error
import Foreign (Foreign, readString)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile) as Sync
import Node.Process (exit) as Process
import Node.Yargs.Applicative (flag, rest, runY, yarg)
import Node.Yargs.Setup (usage)
import Text.Parsing.Parser (runParser)

app :: String -> String -> Boolean -> Array Foreign -> Effect Unit
app strMode package domains fs = launchAff_ $ processResult do
  config <- except do
        mode <- readMode strMode
        files <- readFiles fs
        pure { mode, package, files, domains }
  void $ traverse (processFile config) config.files
  when config.domains do
    pool <- liftEffect $ newPool Database.poolConfiguration
    ds <- Database.domainModule pool
    processModules config "(domains query)" [ ds ]

processResult :: ExceptT String Aff Unit -> Aff Unit
processResult r = do
  e <- runExceptT r
  e # either
    (\s -> liftEffect $ do
      Console.error $ "ERROR: " <> s
      Process.exit 1)
    pure

data Mode
  = Pretty
  | Purs
  | Scala
  | Show
  | Test

type Config =
  { mode :: Mode
  , package :: String
  , files :: Array String
  , domains :: Boolean
  }

readMode :: String -> Either String Mode
readMode = case _ of
  "pretty" -> Right Pretty
  "purs" -> Right Purs
  "scala" -> Right Scala
  "show" -> Right Show
  "test" -> Right Test
  m -> Left $ "Unknown mode " <> m

readFiles :: Array Foreign -> Either String (Array String)
readFiles = lmap show <<< runExcept <<< traverse readString

processFile :: Config -> String -> ExceptT String Aff Unit
processFile config fileName = do
  contents <- withExceptT Error.message
                <<< ExceptT
                <<< liftEffect
                <<< try
                $ Sync.readTextFile UTF8 fileName
  modules <- except $ lmap (errorMessage fileName) (runParser contents wholeFile)
  processModules config fileName modules

processModules :: Config -> String -> Array Module -> ExceptT String Aff Unit
processModules config fileName modules = do
  output <- case config.mode of
    Pretty -> pure $ PrintPrinter.prettyPrint modules
    Purs -> pure $ Purescript.prettyPrint config.package modules
    Scala -> pure $ Scala.prettyPrint config.package modules
    Show -> pure $ show modules
    Test -> do
      b <- except $ lmap (errorMessage fileName) (roundTrip modules)
      if b
        then pure "Round-trip passed"
        else except $ Left "Round-trip failed"
  Console.info output

main :: Effect Unit
main = do
  let setup = usage "$0 --mode mode a.tmpl"
  runY setup $ app <$> yarg
                        "m"
                        [ "mode" ]
                        (Just "The output mode (must be one of pretty, purs, scala, show, or test)")
                        (Right "Mode is required")
                        false
                   <*> yarg
                        "p"
                        [ "package" ]
                        (Just "The package (Scala) or module prefix (PureScript) to use")
                        (Right "Package is required")
                        false
                   <*> flag
                        "d"
                        [ "domains" ]
                        (Just "Query database domains")
                  <*> rest
