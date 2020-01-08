module Test.Ccap.Codegen.Imports
  ( specs
  ) where

import Prelude
import Ccap.Codegen.FileSystem as FS
import Ccap.Codegen.Imports (importsInScope, validateImports)
import Ccap.Codegen.Module (validateModules)
import Ccap.Codegen.TypeRef (validateAllTypeRefs)
import Ccap.Codegen.ValidationError (class ValidationError, printError)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Data.Either (either)
import Data.Foldable (fold)
import Data.Traversable (intercalate, sequence, traverse)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Node.Path as Path
import Test.Ccap.Codegen.Util (eqElems, shouldBeLeft, shouldBeRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)

resources :: FilePath
resources = "./test/resources/"

imports_ :: FilePath
imports_ = Path.concat [ resources, "imports" ]

includes_ :: FilePath
includes_ = Path.concat [ resources, "includes" ]

internal_ :: FilePath
internal_ = Path.concat [ includes_, "internal" ]

external_ :: FilePath
external_ = Path.concat [ includes_, "external" ]

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

externalSubmoduleSource :: FilePath
externalSubmoduleSource = internal "SourceExternalSubmodule.tmpl"

app1 :: FilePath
app1 = Path.concat [ imports_, "app1", "Main.tmpl" ]

app2 :: FilePath
app2 = Path.concat [ imports_, "app2", "Main.tmpl" ]

withPrintErrors ∷ ∀ e m a. ValidationError e ⇒ Monad m ⇒ ExceptT (Array e) m a → ExceptT String m a
withPrintErrors = withExceptT $ fold <<< map printError

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
        imports <-
          liftEffect
            $ runExceptT do
                source <- ExceptT $ FS.sourceFile filePath
                withPrintErrors $ ExceptT $ importsInScope includes source
        shouldBeRight imports

    itCanValidateImports filePath includes =
      it "Can validate it's imports" do
        imports <-
          liftEffect
            $ runExceptT do
                source <- ExceptT $ FS.sourceFile filePath
                withPrintErrors $ ExceptT $ validateImports includes [ source ]
        shouldBeRight imports

    itCanValidateAllModules filePaths includes =
      it "Can validate all modules" do
        modules <-
          liftEffect
            $ runExceptT do
                sources <- ExceptT $ map sequence $ traverse FS.sourceFile filePaths
                withExceptT (intercalate "|") $ ExceptT $ validateModules includes sources
        shouldBeRight modules

    itFailsWithoutIncludes filePath =
      it "Fails validation without including the external folder" do
        let
          includes = []
        imports <-
          liftEffect
            $ runExceptT do
                source <- ExceptT $ FS.sourceFile filePath
                withPrintErrors $ ExceptT $ validateImports includes [ source ]
        shouldBeLeft imports

    itHasValidTypeReferences filePath includes =
      it "Has valid type references to imported types" do
        typeDecls <-
          liftEffect
            $ runExceptT do
                source <- ExceptT $ FS.sourceFile filePath
                imports <- withPrintErrors $ ExceptT $ validateImports includes [ source ]
                withPrintErrors $ except $ validateAllTypeRefs source.contents (imports <#> _.contents.mod)
        shouldBeRight typeDecls
  in
    describe "template include syntax" do
      describe "a plain file with no references" do
        itCanBeParsed plainSource
        itHasImports plainSource []
        itHasValidTypeReferences plainSource []
      describe "a file with an neighboring reference" do
        itCanBeParsed internalSource
        itHasImports internalSource [ "Internal" ]
        itCanFindImports internalSource []
        itCanValidateImports internalSource []
        itHasValidTypeReferences internalSource []
      describe "a file with an submodule reference" do
        itCanBeParsed submoduleSource
        itHasImports submoduleSource [ "submodule.Submodule" ]
        itCanFindImports submoduleSource []
        itCanValidateImports submoduleSource []
        itHasValidTypeReferences submoduleSource []
        itCanValidateAllModules [ submoduleSource ] []
      describe "a file with an external reference" do
        itCanBeParsed externalSource
        itHasImports externalSource [ "External" ]
        itCanFindImports externalSource [ external_ ]
        itCanValidateImports externalSource [ external_ ]
        itFailsWithoutIncludes externalSource
        itHasValidTypeReferences externalSource [ external_ ]
        itCanValidateAllModules [ externalSource ] [ external_ ]
      describe "a file with an external reference to a submodule" do
        itCanBeParsed externalSubmoduleSource
        itHasImports externalSubmoduleSource [ "submodule.ExternalSubmodule" ]
        itCanFindImports externalSubmoduleSource [ external_ ]
        itCanValidateImports externalSubmoduleSource [ external_ ]
        itFailsWithoutIncludes externalSubmoduleSource
        itHasValidTypeReferences externalSubmoduleSource [ external_ ]
        itCanValidateAllModules [ externalSubmoduleSource ] [ external_ ]
      describe "two files with identical relative imports"
        $ itCanValidateAllModules [ app1, app2 ] []
