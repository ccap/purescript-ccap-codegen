module Main
  ( main
  ) where

import Prelude

import Ccap.Codegen.Config (Config, Mode(..), config)
import Ccap.Codegen.Parser (errorMessage, roundTrip, wholeFile)
import Ccap.Codegen.PrettyPrint as PrettyPrint
import Ccap.Codegen.Purescript as Purescript
import Ccap.Codegen.Scala as Scala
import Ccap.Codegen.Shared (OutputSpec)
import Ccap.Codegen.Types (Module)
import Ccap.Codegen.Util (ensureNewline, liftEffectSafely, processResult, scrubEolSpaces)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), except, runExcept, withExceptT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.String as String
import Data.Traversable (for_, scanl, traverse, traverse_)
import Data.Tuple (Tuple(..), snd, uncurry)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Error
import Foreign (readString)
import Foreign.Generic (Foreign)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as Sync
import Node.Yargs.Applicative (rest, runY)
import Node.Yargs.Setup (usage)
import Text.Parsing.Parser (runParser)

app :: Either String Config -> Array Foreign -> Effect Unit
app eConfig fs = launchAff_ $ processResult do
  config <- except eConfig
  files <- except $ readFiles fs
  fileModules <- traverse parseFile files
  let all = fileModules <#> snd
  traverse_ (uncurry $ writeModule config all) fileModules

readFiles :: Array Foreign -> Either String (Array String)
readFiles = lmap show <<< runExcept <<< traverse readString

parseFile :: String -> ExceptT String Aff (Tuple String Module)
parseFile fileName = do
  contents <- withExceptT Error.message
                <<< ExceptT
                <<< liftEffect
                <<< try
                $ Sync.readTextFile UTF8 fileName
  except $ lmap (errorMessage fileName) (map (Tuple fileName) (runParser contents wholeFile))

writeModule :: Config -> Array Module -> String -> Module -> ExceptT String Aff Unit
writeModule config all fileName mod = do
  case config.mode of
    Pretty -> writeOutput config mod PrettyPrint.outputSpec
    Purs -> writeOutput config mod (Purescript.outputSpec config.package all)
    Scala -> writeOutput config mod (Scala.outputSpec config.package all)
    Show -> Console.info $ show mod
    Test -> do
      b <- except $ lmap (errorMessage fileName) (roundTrip mod)
      if b
        then Console.info "Round-trip passed"
        else except $ Left "Round-trip failed"

writeOutput :: Config -> Module -> OutputSpec -> ExceptT String Aff Unit
writeOutput config mod outputSpec = do
  config.outputDirectory # maybe
    (Console.info <<< scrubEolSpaces <<< outputSpec.render $ mod)
    writeOutput_
  where
    writeOutput_ :: String -> ExceptT String Aff Unit
    writeOutput_ dir = do
      let outputFile = String.joinWith "/" filePath
      Console.info $ "Writing " <> outputFile
      ensureDirectoryExists outputFile
      liftEffectSafely $ Sync.writeTextFile
        UTF8
        outputFile
        (ensureNewline <<< scrubEolSpaces <<< outputSpec.render $ mod)
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
main =
  let setup = usage "$0 --package <package> --mode <mode> a.tmpl"
  in runY setup $ app <$> config <*> rest
