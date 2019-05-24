module Main
  ( main
  ) where

import Prelude

import Ccap.Codegen.Parser (errorMessage, roundTrip, wholeFile)
import Ccap.Codegen.PrettyPrint (prettyPrint) as PrintPrinter
import Ccap.Codegen.Purescript (prettyPrint) as Purescript
import Ccap.Codegen.Scala (prettyPrint) as Scala
import Ccap.Codegen.Types (Module)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, info) as Console
import Effect.Exception (message) as Error
import Foreign (readString)
import Foreign.Generic (Foreign)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile) as Sync
import Node.Process (exit) as Process
import Node.Yargs.Applicative (rest, runY, yarg)
import Node.Yargs.Setup (usage)
import Text.Parsing.Parser (runParser)

app :: String -> String -> Array Foreign -> Effect Unit
app strMode package fs = launchAff_ $ processResult do
  config <- except do
        mode <- readMode strMode
        files <- readFiles fs
        pure { mode, package, files }
  void $ traverse (processFile config) config.files

processResult :: ExceptT String Aff Unit -> Aff Unit
processResult r = do
  e <- runExceptT r
  e # either
    (\s -> liftEffect $ do
      Console.error $ "ERROR: " <> s
      Process.exit 1)
    pure

type RunSpec =
  { fileName :: String
  , contents :: String
  }

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
  let runSpec = { fileName, contents }
  pure unit
  case config.mode of
    Pretty -> runParserAndProcess runSpec $
      PrintPrinter.prettyPrint >>> Console.info >>> lift
    Purs -> runParserAndProcess runSpec $
      Purescript.prettyPrint config.package >>> Console.info >>> lift
    Scala -> runParserAndProcess runSpec $
      Scala.prettyPrint config.package >>> Console.info >>> lift
    Show -> runParserAndProcess runSpec $
      show >>> Console.info >>> lift
    Test -> do
      b <- except $ lmap (errorMessage fileName) (roundTrip contents)
      if b
        then lift $ Console.info "Round-trip passed"
        else except $ Left "Round-trip failed"

runParserAndProcess
  :: RunSpec
  -> (Array Module -> ExceptT String Aff Unit)
  -> ExceptT String Aff Unit
runParserAndProcess { fileName, contents } process = do
  ast <- except $ lmap (errorMessage fileName) (runParser contents wholeFile)
  process ast

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
                  <*> rest
