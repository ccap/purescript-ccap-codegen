module Main
  ( main
  ) where

import Prelude

import Ccap.Codegen.Parser (errorMessage, roundTrip, wholeFile)
import Ccap.Codegen.PrettyPrint as PrettyPrint
import Ccap.Codegen.Purescript as Purescript
import Ccap.Codegen.Scala as Scala
import Ccap.Codegen.Shared (OutputSpec)
import Ccap.Codegen.Types (Module)
import Ccap.Codegen.Util (liftEffectSafely, processResult, scrubEolSpaces)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), except, runExcept, withExceptT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Traversable (for_, scanl, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Error
import Foreign (readString)
import Foreign.Generic (Foreign)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as Sync
import Node.Yargs.Applicative (rest, runY, yarg)
import Node.Yargs.Setup (usage)
import Text.Parsing.Parser (runParser)

app :: String -> String -> String -> Array Foreign -> Effect Unit
app strMode package outputDirectoryParam fs = launchAff_ $ processResult do
  let checkString s =
        if String.length s > 0
          then Just s
          else Nothing
      outputDirectory = checkString outputDirectoryParam

  config <- except do
        mode <- readMode strMode
        files <- readFiles fs
        pure { mode, package, files, outputDirectory }

  fileModules <- traverse (processFile config) config.files

  let all = fileModules >>= \(Tuple fileName modules) -> modules

  for_ fileModules \(Tuple fileName modules) ->
    processModules config fileName modules all

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

processModules :: Config -> String -> Array Module -> Array Module -> ExceptT String Aff Unit
processModules config fileName modules all = do
  case config.mode of
    Pretty -> writeOutput config modules PrettyPrint.outputSpec
    Purs -> writeOutput config modules (Purescript.outputSpec config.package all)
    Scala -> writeOutput config modules (Scala.outputSpec config.package all)
    Show -> Console.info $ show modules
    Test -> do
      b <- except $ lmap (errorMessage fileName) (roundTrip modules)
      if b
        then Console.info "Round-trip passed"
        else except $ Left "Round-trip failed"

writeOutput :: Config -> Array Module -> OutputSpec -> ExceptT String Aff Unit
writeOutput config modules outputSpec = do
  for_ modules (\mod ->
    config.outputDirectory # maybe
      (Console.info <<< scrubEolSpaces <<< outputSpec.render $ mod)
      (writeOutput_ mod))
  where
    writeOutput_ :: Module -> String -> ExceptT String Aff Unit
    writeOutput_ mod dir = do
      let outputFile = String.joinWith "/" filePath
      Console.info $ "Writing " <> outputFile
      ensureDirectoryExists outputFile
      liftEffectSafely $ Sync.writeTextFile
        UTF8
        outputFile
        (scrubEolSpaces <<< outputSpec.render $ mod)
      where
        filePath = [ dir, (outputSpec.filePath mod) ]

ensureDirectoryExists :: String -> ExceptT String Aff Unit
ensureDirectoryExists filePath =
  dirPath filePath # maybe (pure unit) \dir -> do
    let parts = Array.filter (not <<< String.null) (String.split (String.Pattern "/") dir)
    for_ (outputDirectories (String.take 1 filePath == "/") parts) \d ->
      liftEffectSafely do
        ifM (Sync.exists d)
          (pure unit)
          (Sync.mkdir d)
  where
    dirPath :: String -> Maybe String
    dirPath p =
      let idx = String.lastIndexOf (String.Pattern "/") p
      in map (flip String.take filePath) idx

    outputDirectories :: Boolean -> Array String -> Array String
    outputDirectories rootPath parts =
      scanl
        (\b a -> if b == "" then a else b <> "/" <> a)
        ""
        (if rootPath then fromMaybe parts (Array.modifyAt 0 ("/" <> _) parts) else parts)

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
                   <*> yarg
                        "o"
                        [ "output-directory" ]
                        (Just "Files will be written to this directory")
                        (Left "")
                        true
                  <*> rest
