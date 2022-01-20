module Test.Ccap.Codegen.Exports
  ( specs
  ) where

import Prelude
import Ccap.Codegen.PureScript as PureScript
import Ccap.Codegen.Scala as Scala
import Ccap.Codegen.Shared (OutputSpec)
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Test.Ccap.Codegen.Util (exceptAffT, matchKeyLine, runOrFail, sourceAstTmpl)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

tmplFile :: FilePath
tmplFile = "./test/resources/exports/Exports.tmpl"

importFile :: FilePath
importFile = "./test/resources/exports/Imports.tmpl"

specs :: Spec Unit
specs =
  describe "Exports" do
    describe "Scala exports" do
      it "only uses exports.scalaPkg for file path"
        $ matchOutputPath Scala.outputSpec "test/Exports.scala"
      it "uses the parent directory as the package"
        $ matchKeyLine_ "package" Scala.outputSpec "package test"
      it "uses the last name as the main object name"
        $ matchKeyLine_ "object" Scala.outputSpec "object Exports {"
    describe "Purescript exports" do
      it "only uses exports.pursPkg for file path"
        $ matchOutputPath PureScript.outputSpec "Test/Exports.purs"
      it "uses the pursPkg for the module path"
        $ matchKeyLine_ "module" PureScript.outputSpec "module Test.Exports where"
    describe "Imports of custom Exports" do
      let
        check = matchKeyLine importFile "ImportedType"
      describe "Scala imports" do
        it "References type with it's scala object name"
          $ check Scala.outputSpec "  type ImportedType = Exports.ExportedType"
        it "Uses the imported module qualifier when defining record fields"
          $ matchKeyLine importFile "field" Scala.outputSpec "  field: Exports.ExportedType,"
        it "Won't prefix if it's the imported file's class"
          $ matchKeyLine importFile "ImportedRec" Scala.outputSpec "  type ImportedRec = Exports"
      describe "Purescript imports" do
        it "References with it's purescript module name"
          $ check PureScript.outputSpec "type ImportedType = Exports.ExportedType"

matchOutputPath :: OutputSpec -> FilePath -> Aff Unit
matchOutputPath outSpec outPath =
  runOrFail do
    source <- exceptAffT $ sourceAstTmpl tmplFile
    let
      path = outSpec.filePath source.contents
    pure $ path `shouldEqual` outPath

matchKeyLine_ :: String -> OutputSpec -> String -> Aff Unit
matchKeyLine_ = matchKeyLine tmplFile
