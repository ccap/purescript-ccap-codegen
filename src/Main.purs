module Main
  ( main
  ) where

import Prelude
import Ccap.Codegen.Ast as Ast
import Ccap.Codegen.AstBuilder as AstBuilder
import Ccap.Codegen.Config (Config, Mode(..), config)
import Ccap.Codegen.Cst as Cst
import Ccap.Codegen.Error as Error
import Ccap.Codegen.FileSystem (mkDirP)
import Ccap.Codegen.Parser as Parser
import Ccap.Codegen.PrettyPrint as PrettyPrint
import Ccap.Codegen.PureScript as PureScript
import Ccap.Codegen.PureScriptJs as PureScriptJs
import Ccap.Codegen.Scala as Scala
import Ccap.Codegen.Shared (OutputSpec)
import Ccap.Codegen.Util (ensureNewline, liftEffectSafely, processResult, scrubEolSpaces)
import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, maybe)
import Data.String as String
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as Sync
import Node.Path (FilePath)
import Node.Path as Path
import Options.Applicative as OptParse

app :: Config -> Effect Unit
app config =
  launchAff_
    $ processResult
    $ writeModules config config.targetFiles

writeModules :: Config -> Array FilePath -> ExceptT String Aff Unit
writeModules config files = case config.mode of
  Pretty -> do
    modules <- convertBuilderResult (AstBuilder.parseAll files)
    traverse_ (Console.info <<< scrubEolSpaces <<< PrettyPrint.prettyPrint <<< _.contents) modules
  Purs -> do
    modules <- convertBuilderResult (AstBuilder.build { files, importPaths: config.includes })
    traverse_ (writeOutput config PureScript.outputSpec) modules
  PursJs -> do
    modules <- convertBuilderResult (AstBuilder.build { files, importPaths: config.includes })
    traverse_ (writeOutput config PureScriptJs.outputSpec) modules
  Scala -> do
    modules <- convertBuilderResult (AstBuilder.build { files, importPaths: config.includes })
    traverse_ (writeOutput config Scala.outputSpec) modules
  Show -> do
    modules <- convertBuilderResult (AstBuilder.build { files, importPaths: config.includes })
    traverse_ (Console.info <<< show) modules
  Test -> do
    modules <- convertBuilderResult (AstBuilder.parseAll files)
    for_ modules \mod ->
      ifM (except (lmap Parser.errorMessage (Parser.roundTrip mod)))
        (Console.info "Round-trip passed")
        (except $ Left "Round-trip failed")

convertBuilderResult :: ExceptT (NonEmptyArray Error.Error) Effect ~> ExceptT String Aff
convertBuilderResult e = ExceptT (liftEffect (map (lmap errorToOutputString) (runExceptT e)))
  where
  errorToOutputString :: NonEmptyArray Error.Error -> String
  errorToOutputString es =
    Error.toString (NonEmptyArray.head es) <> "\n"
      <> String.joinWith "\n" (map (\ee -> "ERROR: " <> (Error.toString ee)) (NonEmptyArray.tail es))

writeOutput :: Config -> OutputSpec -> Cst.Source Ast.Module -> ExceptT String Aff Unit
writeOutput config outputSpec mod = do
  config.outputDirectory
    # maybe
        (Console.info <<< scrubEolSpaces <<< fromMaybe "" <<< outputSpec.render $ mod.contents)
        writeOutput_
  where
  writeOutput_ :: String -> ExceptT String Aff Unit
  writeOutput_ dir = do
    let
      filePath = [ dir, (outputSpec.filePath mod.contents) ]

      outputFile = Path.concat filePath

      contents = outputSpec.render mod.contents
    maybe
      (Console.info ("Skipping " <> outputFile))
      ( \c -> do
          Console.info $ "Writing " <> outputFile
          mkDirP (Path.dirname outputFile)
          liftEffectSafely
            $ Sync.writeTextFile
                UTF8
                outputFile
                (ensureNewline <<< scrubEolSpaces $ c)
      )
      contents

main :: Effect Unit
main = do
  configuration <- OptParse.execParser $ OptParse.info config (OptParse.fullDesc)
  app configuration
