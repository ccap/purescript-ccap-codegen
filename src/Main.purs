module Main where

import Prelude

import Ccap.Codegen.Parser (errorMessage, roundTrip, wholeFile)
import Ccap.Codegen.PrettyPrint (prettyPrint) as PrintPrinter
import Ccap.Codegen.Purescript (prettyPrint) as Purescript
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

app :: String -> String -> Effect Unit
app fileName mode = do
  contents <- Sync.readTextFile UTF8 fileName
  case mode of
    "pretty" -> runParserAndProcess fileName contents \ms ->
      Console.info $ PrintPrinter.prettyPrint ms
    "purs" -> runParserAndProcess fileName contents $
      Purescript.prettyPrint >>> Console.info
    "test" -> do
      let success = roundTrip contents
      either
        (Console.error <<< errorMessage fileName)
        (\b -> Console.info $ "Round-trip returned " <> show b)
        success
    m -> Console.error $ "ERROR: Unknown mode " <> m

runParserAndProcess
  :: String
  -> String
  -> (Array Module -> Effect Unit)
  -> Effect Unit
runParserAndProcess fileName contents process = do
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
                        (Just "The output mode (must be one of pretty, scala, ps, or test)")
                        (Right "Mode is required")
                        false
