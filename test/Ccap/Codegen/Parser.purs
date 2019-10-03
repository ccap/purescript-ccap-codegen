module Test.Ccap.Codegen.Parser
  ( specs
  ) where

import Prelude

import Ccap.Codegen.FileSystem (joinPaths, readTextFile)
import Ccap.Codegen.PrettyPrint (prettyPrint)
import Ccap.Codegen.Purescript as Purescript
import Ccap.Codegen.Scala as Scala
import Ccap.Codegen.Types (Source, ValidatedModule)
import Ccap.Codegen.Util (ensureNewline, scrubEolSpaces)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Except.Trans (except)
import Data.Array as Array
import Data.Either (Either, either)
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Test.Ccap.Codegen.Util (diffByLine, parse, validateModule)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

parseDir :: FilePath
parseDir = "./test/resources/parser"

base :: FilePath
base = joinPaths parseDir "Printed"

tmplFile :: FilePath
tmplFile = base <> ".tmpl"

scalaFile :: FilePath
scalaFile = base <> ".scala"

pursFile :: FilePath
pursFile = base <> ".purs_" -- purs_ so pulp doesn't try to compile it.

importedFile :: FilePath
importedFile = joinPaths parseDir "Imported.tmpl"

specs :: Spec Unit
specs = describe "The .tmpl file parser" do
  it "Pretty prints a file is parsed as an identical Module" do
    results <-
      liftEffect $ runExceptT do
        validated <- ExceptT $ sourceTmpl tmplFile
        let printed = scrubEolSpaces $ prettyPrint validated.contents
        resourced <- except $ parse tmplFile printed
        revalidated <- ExceptT $ validateModule resourced
        pure $ Tuple validated revalidated
    either fail (uncurry compareModules) results
  it "Prints a scala a file as expected" do
    results <-
      liftEffect $ runExceptT do
        validated <- ExceptT $ sourceTmpl tmplFile
        let
          spec = Scala.outputSpec "Test"
          printed = ensureNewline $ scrubEolSpaces $ spec.render validated.contents
        scala <- ExceptT $ readTextFile scalaFile
        pure $ Tuple printed scala
    either fail (uncurry diffByLine) results
  it "Prints a purs a file as expected" do
    results <-
      liftEffect $ runExceptT do
        validated <- ExceptT $ sourceTmpl tmplFile
        let
          spec = Purescript.outputSpec "Test"
          printed = ensureNewline $ scrubEolSpaces $ spec.render validated.contents
        purs <- ExceptT $ readTextFile pursFile
        pure $ Tuple printed purs
    either fail (uncurry diffByLine) results

sourceTmpl :: FilePath -> Effect (Either String (Source ValidatedModule))
sourceTmpl filePath = runExceptT do
  text <- ExceptT $ readTextFile filePath
  sourced <- except $ parse filePath text
  ExceptT $ validateModule sourced

compareModules :: Source ValidatedModule -> Source ValidatedModule -> Aff Unit
compareModules x y = do
  x.source `shouldEqual` y.source
  x.contents.name `shouldEqual` y.contents.name
  x.contents.annots `shouldEqual` y.contents.annots
  x.contents.imports `shouldEqual` y.contents.imports
  x.contents.exports `shouldEqual` y.contents.exports
  (Array.length x.contents.types) `shouldEqual` (Array.length y.contents.types)
  let types = Array.zip x.contents.types y.contents.types
  traverse_ (uncurry shouldEqual) types

