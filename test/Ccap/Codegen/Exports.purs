module Test.Ccap.Codegen.Exports
  ( specs
  ) where

import Prelude

import Ccap.Codegen.Purescript as Purescript
import Ccap.Codegen.Scala as Scala
import Ccap.Codegen.Shared (OutputSpec)
import Data.Maybe (fromMaybe)
import Data.String.Utils as String
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Test.Ccap.Codegen.Util (exceptAffT, findLine, print, runOrFail, sourceTmpl)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

tmplFile :: FilePath
tmplFile = "./test/resources/exports/Exports.tmpl"

specs :: Spec Unit
specs = describe "Exports" do
  describe "Scala exports" do
    it "only uses exports.scalaPkg for file path" $
      matchOutputPath Scala.outputSpec "test/ScalaExport.scala"
    it "uses the parent directory as the package" $
      matchKeyLine "package" Scala.outputSpec "package test"
    it "uses the last name as the main object name" $
      matchKeyLine "object" Scala.outputSpec "object ScalaExport {"
  describe "Purescript exports" do
    it "only uses exports.pursPkg for file path" $
      matchOutputPath Purescript.outputSpec "Test/PurescriptExport.purs"
    it "uses the pursPkg for the module path" $
      matchKeyLine "module" Purescript.outputSpec "module Test.PurescriptExport where"

matchOutputPath :: OutputSpec -> FilePath -> Aff Unit
matchOutputPath outSpec outPath = runOrFail do
  source <- exceptAffT $ sourceTmpl tmplFile
  let path = outSpec.filePath source.contents
  pure $ path `shouldEqual` outPath

matchKeyLine :: String -> OutputSpec -> String -> Aff Unit
matchKeyLine keyWord outSpec line = runOrFail do
  source <- exceptAffT $ sourceTmpl tmplFile
  let printed = print outSpec source
      keyLine = findLine (String.startsWith keyWord) printed
        # fromMaybe ("Keyword, " <> keyWord <> ", not found")
  pure $ keyLine `shouldEqual` line
