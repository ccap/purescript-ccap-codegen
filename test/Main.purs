module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Ccap.Codegen.Annotations (specs) as Annotations
import Test.Ccap.Codegen.Exports (specs) as Exports
import Test.Ccap.Codegen.FileSystem (specs) as FileSystem
import Test.Ccap.Codegen.Imports (specs) as Imports
import Test.Ccap.Codegen.Parser (specs) as Parser
import Test.Ccap.Codegen.Prefix (specs) as Prefix
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        FileSystem.specs
        Imports.specs
        Parser.specs
        Exports.specs
        Annotations.specs
        Prefix.specs
