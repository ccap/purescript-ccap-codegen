module Test.Ccap.Codegen.Parser
  ( specs
  ) where

import Prelude
import Ccap.Codegen.Cst as Cst
import Ccap.Codegen.FileSystem (joinPaths, readTextFile)
import Ccap.Codegen.PrettyPrint as PrettyPrint
import Ccap.Codegen.PureScript as PureScript
import Ccap.Codegen.Scala as Scala
import Ccap.Codegen.Util (scrubEolSpaces)
import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (either)
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Test.Ccap.Codegen.Util (diffByLine, parse, print, sourceAstTmpl, sourceCstTmpl)
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
specs =
  describe "The .tmpl file parser" do
    it "Pretty prints a file is parsed as an identical Module" do
      results <-
        liftEffect
          $ runExceptT do
              input <- ExceptT $ sourceCstTmpl tmplFile
              let
                printed = scrubEolSpaces (PrettyPrint.prettyPrint input.contents)
              resourced <- except $ parse tmplFile printed
              pure $ Tuple input resourced
      either fail (uncurry compareModules) results
    it "Prints a scala a file as expected" do
      results <-
        liftEffect
          $ runExceptT do
              validated <- ExceptT $ sourceAstTmpl tmplFile
              let
                printed = print Scala.outputSpec validated
              scala <- ExceptT $ readTextFile scalaFile
              pure $ Tuple printed scala
      either fail (uncurry diffByLine) results
    it "Prints a purs a file as expected" do
      results <-
        liftEffect
          $ runExceptT do
              validated <- ExceptT $ sourceAstTmpl tmplFile
              let
                printed = print PureScript.outputSpec validated
              purs <- ExceptT $ readTextFile pursFile
              pure $ Tuple printed purs
      either fail (uncurry diffByLine) results

compareModules :: Cst.Source Cst.Module -> Cst.Source Cst.Module -> Aff Unit
compareModules x y = do
  x.source `shouldEqual` y.source
  x.contents.imports `shouldEqual` y.contents.imports
  x.contents.exports `shouldEqual` y.contents.exports
  (NonEmptyArray.length x.contents.types) `shouldEqual` (NonEmptyArray.length y.contents.types)
  let
    types = NonEmptyArray.zip x.contents.types y.contents.types
  traverse_ (uncurry shouldEqual) types
