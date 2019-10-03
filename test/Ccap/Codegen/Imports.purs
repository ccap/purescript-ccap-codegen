module Test.Ccap.Codegen.Imports
  ( specs
  ) where

import Prelude

import Ccap.Codegen.FileSystem as FS
import Ccap.Codegen.Imports (importsInScope)
import Ccap.Codegen.ValidationError (printError)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Data.Array (sort)
import Data.Either (Either, either, isRight)
import Data.Foldable (intercalate)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Node.Path (FilePath)
import Node.Path as Path
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)

shouldBeRight :: forall m a b. MonadThrow Error m => Show a => Show b => Either a b -> m Unit
shouldBeRight = flip shouldSatisfy isRight

eqElems :: forall a. Ord a => Eq a => Array a -> Array a -> Boolean
eqElems xs ys = sort xs == sort ys

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
        <<< (shouldBeRight <=< liftEffect <<< FS.parseFile)
    itHasImports source imports =
      it "parsed the imports as expected" do
        sourceImports <- FS.parseFile source <#> (map _.contents.imports) # liftEffect
        shouldSatisfy sourceImports $ either (const false) (eqElems imports)
    itCanFindImports source includes =
      it "Has imports that exist" do
        imports <- liftEffect $ runExceptT do
          mod <- ExceptT $ FS.parseFile source
          withExceptT (intercalate "\n" <<< map printError)
            $ ExceptT
            $ importsInScope includes mod
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
      describe "a file with an submodule reference" do
        itCanBeParsed submoduleSource
        itHasImports submoduleSource [ "submodule.Submodule" ]
        itCanFindImports submoduleSource []
      describe "a file with an external reference" do
        itCanBeParsed externalSource
        itHasImports externalSource [ "External" ]
        itCanFindImports externalSource [ external_ ]
