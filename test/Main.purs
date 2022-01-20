module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Ccap.Codegen.Annotations (specs) as Annotations
import Test.Ccap.Codegen.Exports (specs) as Exports
import Test.Ccap.Codegen.FileSystem (specs) as FileSystem
import Test.Ccap.Codegen.FastDecoding (specs) as FastDecoding
import Test.Ccap.Codegen.Parser (specs) as Parser
import Test.Ccap.Codegen.Prefix (specs) as Prefix
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        FastDecoding.specs
        FileSystem.specs
        Parser.specs
        Exports.specs
        Annotations.specs
        Prefix.specs
