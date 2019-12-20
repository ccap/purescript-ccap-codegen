module Test.Ccap.Codegen.Prefix
  ( specs
  ) where

import Prelude
import Ccap.Codegen.Scala as Scala
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Test.Ccap.Codegen.Util (matchKeyLine)
import Test.Spec (Spec, describe, it)

tmplFile :: FilePath
tmplFile = "./test/resources/prefix/Prefix.tmpl"

specs :: Spec Unit
specs =
  describe "Scala Prefixes" do
    describe "External references" do
      describe "To a class" do
        describe "When they are the file's class" do
          it "Are imported"
            $ check "import external" "import external.Imported"
          it "Are not prefixed since they are imported"
            $ check "type ExternalClassRef" "  type ExternalClassRef = Imported"
        it "Need prefixes if they aren't the file's class"
          $ check "type ExternalRecRef" "  type ExternalRecRef = Imported.ImportedRec"
      describe "Type references" do
        it "Need prefixes"
          $ check "type ExternalTypeRef" "  type ExternalTypeRef = Imported.ImportedType"
    describe "Internal references" do
      describe "To class definitions" do
        it "Need prefixes if they are the file's class"
          $ check "prefixInternalRef:" "  prefixInternalRef: Prefix.CustomType,"
        it "Don't need prefixes if defined in the companion object"
          $ check "customInternalRef:" "    customInternalRef: CustomType,"
      describe "type references" do
        it "Are defined in the companion object and don't need prefixes"
          $ check "type InternalRef" "  type InternalRef = CustomType"

check :: String -> String -> Aff Unit
check keyword = matchKeyLine tmplFile keyword Scala.outputSpec
