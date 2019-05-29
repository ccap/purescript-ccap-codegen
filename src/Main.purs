module Main
  ( main
  ) where

import Prelude

import Ccap.Codegen.Database as Database
import Ccap.Codegen.Parser (errorMessage, roundTrip, wholeFile)
import Ccap.Codegen.PrettyPrint as PrettyPrint
import Ccap.Codegen.Purescript as Purescript
import Ccap.Codegen.Scala as Scala
import Ccap.Codegen.Shared (OutputSpec)
import Ccap.Codegen.Types (Module)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT, withExceptT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String as String
import Data.String.Regex (regex)
import Data.String.Regex (replace) as Regex
import Data.String.Regex.Flags (global, multiline) as Regex.Flags
import Data.Traversable (for_, scanl, traverse)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL.PG (newPool)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Error
import Foreign (readString)
import Foreign.Generic (Foreign)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as Sync
import Node.Process as Process
import Node.Yargs.Applicative (flag, rest, runY, yarg)
import Node.Yargs.Setup (usage)
import Text.Parsing.Parser (runParser)

app :: String -> String -> Boolean -> String -> String -> Array Foreign -> Effect Unit
app strMode package domains tableParam outputDirectoryParam fs = launchAff_ $ processResult do
  let checkString s =
        if String.length s > 0
          then Just s
          else Nothing
      table = checkString tableParam
      outputDirectory = checkString outputDirectoryParam

  config <- except do
        mode <- readMode strMode
        files <- readFiles fs
        pure { mode, package, files, domains, table, outputDirectory }

  fileModules <- traverse (processFile config) config.files
  fromDb <- dbModules config

  for_ (Compose $ map outputDirectories (outputPath config)) \d ->
    liftEffectSafely do
      ifM (Sync.exists d)
        (pure unit)
        (Sync.mkdir d)

  for_ (fileModules <> fromDb) \(Tuple fileName modules) ->
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
            t_ <- Database.tableModule pool t
            pure $ [ Tuple ("(" <> t <> " table query)") [ t_ ] ])
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
  , table :: Maybe String
  , outputDirectory :: Maybe String
  }

readMode :: String -> Either String Mode
readMode = case _ of
  "pretty" -> Right Pretty
  "purs" -> Right Purs
  "scala" -> Right Scala
  "show" -> Right Show
  "test" -> Right Test
  m -> Left $ "Unknown mode " <> show m

readFiles :: Array Foreign -> Either String (Array String)
readFiles = lmap show <<< runExcept <<< traverse readString

processFile :: Config -> String -> ExceptT String Aff (Tuple String (Array Module))
processFile config fileName = do
  contents <- withExceptT Error.message
                <<< ExceptT
                <<< liftEffect
                <<< try
                $ Sync.readTextFile UTF8 fileName
  except $ lmap (errorMessage fileName) (map (Tuple fileName) (runParser contents wholeFile))

processModules :: Config -> String -> Array Module -> ExceptT String Aff Unit
processModules config fileName modules = do
  case config.mode of
    Pretty -> writeOutput config modules PrettyPrint.outputSpec
    Purs -> writeOutput config modules (Purescript.outputSpec config.package)
    Scala -> writeOutput config modules (Scala.outputSpec config.package)
    Show -> Console.info $ show modules
    Test -> do
      b <- except $ lmap (errorMessage fileName) (roundTrip modules)
      if b
        then Console.info "Round-trip passed"
        else except $ Left "Round-trip failed"

liftEffectSafely :: forall a. Effect a -> ExceptT String Aff a
liftEffectSafely = ExceptT <<< liftEffect <<< map (lmap Error.message) <<< try

outputPath :: Config -> Maybe (Array String)
outputPath config =
  config.outputDirectory <#> \o ->
    let packagePath = String.replaceAll
          (String.Pattern ".")
          (String.Replacement "/")
          config.package
    in Array.filter
        (\s -> String.length s > 0)
        (String.split (String.Pattern "/") (o <> "/" <> packagePath))

outputDirectories :: Array String -> Array String
outputDirectories = scanl (\b a -> if b == "" then a else b <> "/" <> a) ""

writeOutput :: Config -> Array Module -> OutputSpec -> ExceptT String Aff Unit
writeOutput config modules outputSpec = liftEffectSafely do
  for_ modules (\mod ->
    outputPath config # maybe
      (Console.info <<< scrubEolSpaces <<< outputSpec.render $ mod)
      (writeOutput_ mod))
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
  runY setup $ app <$> yarg
                        "m"
                        [ "mode" ]
                        (Just "The output mode (must be one of pretty, purs, scala, show, or test)")
                        (Right "Mode is required")
                        true
                   <*> yarg
                        "p"
                        [ "package" ]
                        (Just "The package (Scala) or module prefix (PureScript) to use")
                        (Right "Package is required")
                        true
                   <*> flag
                        "d"
                        [ "domains" ]
                        (Just "Query database domains")
                   <*> yarg
                        "t"
                        [ "table" ]
                        (Just "Query the provided database table")
                        (Left "")
                        true
                   <*> yarg
                        "o"
                        [ "output-directory" ]
                        (Just "Files will be written to this directory")
                        (Left "")
                        true
                  <*> rest
