module Main where

import Prelude

import Ccap.Codegen.Parser (errorMessage, roundTrip, wholeFile)
import Ccap.Codegen.PrettyPrint (prettyPrint) as PrintPrinter
import Ccap.Codegen.Purescript (prettyPrint) as Purescript
import Ccap.Codegen.Scala (prettyPrint) as Scala
import Ccap.Codegen.Types (Module)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error, info) as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile) as Sync
import Node.Yargs.Applicative (runY, yarg)
import Node.Yargs.Setup (usage)
import Text.Parsing.Parser (runParser)

app :: String -> String -> String -> Effect Unit
app fileName mode package = do
  contents <- Sync.readTextFile UTF8 fileName
  let runSpec = { fileName, contents }
  case mode of
    "pretty" -> runParserAndProcess runSpec $
      PrintPrinter.prettyPrint >>> Console.info
    "purs" -> runParserAndProcess runSpec $
      Purescript.prettyPrint package >>> Console.info
    "scala" -> runParserAndProcess runSpec $
      Scala.prettyPrint package >>> Console.info
    "show" -> runParserAndProcess runSpec $
      show >>> Console.info
    "test" -> do
      let success = roundTrip contents
      either
        (Console.error <<< errorMessage fileName)
        (\b -> Console.info $ "Round-trip returned " <> show b)
        success
    m -> Console.error $ "ERROR: Unknown mode " <> m

type RunSpec =
  { fileName :: String
  , contents :: String
  }

runParserAndProcess
  :: RunSpec
  -> (Array Module -> Effect Unit)
  -> Effect Unit
runParserAndProcess { fileName, contents } process = do
  let ast = runParser contents wholeFile
  either
    (Console.error <<< errorMessage fileName)
    process
    ast

main :: Effect Unit
main = do
  let setup = usage "$0 --file a.tmpl --mode mode"
  runY setup $ app <$> yarg
                        "f"
                        [ "file" ]
                        (Just "A template file to process")
                        (Right "At least one file is required")
                        false
                   <*> yarg
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
