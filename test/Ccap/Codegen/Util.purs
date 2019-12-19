module Test.Ccap.Codegen.Util
  ( diffByLine
  , eqElems
  , exceptAffT
  , findLine
  , matchKeyLine
  , onlyOne
  , parse
  , print
  , runOrFail
  , shouldBeLeft
  , shouldBeRight
  , sourceTmpl
  , splitLines
  --  , traceFile
  , validateModule
  ) where

import Prelude
import Ccap.Codegen.FileSystem (readTextFile)
import Ccap.Codegen.Module as Module
import Ccap.Codegen.Parser (errorMessage, parseSource)
import Ccap.Codegen.Shared (OutputSpec)
import Ccap.Codegen.Types (Module, Source, ValidatedModule)
import Ccap.Codegen.Util (ensureNewline, scrubEolSpaces)
import Ccap.Codegen.ValidationError (joinErrors)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Data.Array (sort)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, isLeft, isRight)
import Data.Foldable (traverse_)
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy (find)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..))
import Data.String (split) as String
import Data.String.Utils (includes) as String
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Node.Path (FilePath)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)

exceptAffT :: forall a b. Effect (Either a b) -> ExceptT a Aff b
exceptAffT = ExceptT <<< liftEffect

runOrFail :: ExceptT String Aff (Aff Unit) -> Aff Unit
runOrFail = either fail identity <=< runExceptT

validateModule :: Source Module -> Effect (Either String (Source ValidatedModule))
validateModule = Array.singleton >>> Module.validateModules [] >>> map (joinErrors >=> onlyOne)

onlyOne :: forall a. Array a -> Either String a
onlyOne =
  List.fromFoldable
    >>> case _ of
        Cons x Nil -> Right x
        Nil -> Left "Array is empty"
        Cons x xs -> Left "Array has multiple elements"

shouldBeRight :: forall m a b. MonadThrow Error m => Show a => Show b => Either a b -> m Unit
shouldBeRight = flip shouldSatisfy isRight

shouldBeLeft :: forall m a b. MonadThrow Error m => Show a => Show b => Either a b -> m Unit
shouldBeLeft = flip shouldSatisfy isLeft

eqElems :: forall a. Ord a => Eq a => Array a -> Array a -> Boolean
eqElems xs ys = sort xs == sort ys

parse :: FilePath -> String -> Either String (Source Module)
parse filePath = lmap (errorMessage filePath) <<< parseSource filePath

print :: OutputSpec -> Source ValidatedModule -> String
print { render } { contents } = ensureNewline $ scrubEolSpaces $ render contents

splitLines :: String -> Array String
splitLines = String.split (Pattern "\n")

diffByLine :: String -> String -> Aff Unit
diffByLine x y = do
  let
    xs = splitLines x

    ys = splitLines y

    xys = Array.zip xs ys
  traverse_ (uncurry shouldEqual) xys
  (Array.length xs) `shouldEqual` (Array.length ys)

findLine :: (String -> Boolean) -> String -> Maybe String
findLine pred = find pred <<< splitLines

--traceFile :: forall m. Monad m => String -> m Unit
--traceFile = traverse_ traceM <<< splitLines
sourceTmpl :: FilePath -> Effect (Either String (Source ValidatedModule))
sourceTmpl filePath =
  runExceptT do
    text <- ExceptT $ readTextFile filePath
    sourced <- except $ parse filePath text
    ExceptT $ validateModule sourced

matchKeyLine :: FilePath -> String -> OutputSpec -> String -> Aff Unit
matchKeyLine file keyWord outSpec line =
  runOrFail do
    source <- exceptAffT $ sourceTmpl file
    let
      printed = print outSpec source

      keyLine =
        findLine (String.includes keyWord) printed
          # fromMaybe ("Keyword, " <> keyWord <> ", not found")
    pure $ keyLine `shouldEqual` line
