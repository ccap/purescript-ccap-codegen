module Test.Ccap.Codegen.Imports
  ( specs
  ) where

import Prelude

import Ccap.Codegen.FileSystem as FS
import Ccap.Codegen.Imports (importsInScope, validateImports)
import Ccap.Codegen.ValidationError (printError)
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Data.Either (either)
import Data.Foldable (fold)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Node.Path as Path
import Test.Ccap.Codegen.Util (eqElems, shouldBeLeft, shouldBeRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)

root :: FilePath
root = "./test/resources/includes/"

internal_ :: FilePath
internal_ = Path.concat [ root, "internal" ]

external_ :: FilePath
external_ = Path.concat [ root, "external" ]

internal :: FilePath -> FilePath
internal fileName = Path.concat [ internal_, fileName ]

plainSource :: FilePath
plainSource = internal "SourcePlain.tmpl"

internalSource :: FilePath
internalSource = internal "SourceInternal.tmpl"

submoduleSource :: FilePath
submoduleSource = internal "SourceSubmodule.tmpl"

externalSource :: FilePath
externalSource = internal "SourceExternal.tmpl"

allSources :: Array FilePath
allSources = [ plainSource, internalSource, submoduleSource, externalSource ]

specs :: Spec Unit
specs =
  let
    itCanBeParsed =
      it "can be parsed with no errors"
        <<< (shouldBeRight <=< liftEffect <<< FS.sourceFile)
    itHasImports filePath imports =
      it "parsed the imports as expected" do
        sourceImports <- FS.sourceFile filePath <#> (map _.contents.imports) # liftEffect
        shouldSatisfy sourceImports $ either (const false) (eqElems imports)
    itCanFindImports filePath includes =
      it "Has imports that exist" do
        imports <- liftEffect $ runExceptT do
          source <- ExceptT $ FS.sourceFile filePath
          withExceptT (fold <<< map printError)
            $ ExceptT
            $ importsInScope includes source
        shouldBeRight imports
    itCanValidateImports filePath includes =
      it "Can validate it's imports" do
        imports <- liftEffect $ runExceptT do
          source <- ExceptT $ FS.sourceFile filePath
          withExceptT (fold <<< map printError)
            $ ExceptT
            $ validateImports includes [ source ]
        shouldBeRight imports
  in
    describe "template include syntax" do
      describe "a plain file with no references" do
        itCanBeParsed plainSource
        itHasImports plainSource []
      describe "a file with an neighboring reference" do
        itCanBeParsed internalSource
        itHasImports internalSource [ "Internal" ]
        itCanFindImports internalSource []
        itCanValidateImports internalSource []
      describe "a file with an submodule reference" do
        itCanBeParsed submoduleSource
        itHasImports submoduleSource [ "submodule.Submodule" ]
        itCanFindImports submoduleSource []
        itCanValidateImports submoduleSource []
      describe "a file with an external reference" do
        itCanBeParsed externalSource
        itHasImports externalSource [ "External" ]
        itCanFindImports externalSource [ external_ ]
        itCanValidateImports externalSource [ external_ ]
        it "Fails validation without including the external folder" do
          let includes = []
          imports <- liftEffect $ runExceptT do
            source <- ExceptT $ FS.sourceFile externalSource
            withExceptT (fold <<< map printError)
              $ ExceptT
              $ validateImports includes [ source ]
          shouldBeLeft imports
