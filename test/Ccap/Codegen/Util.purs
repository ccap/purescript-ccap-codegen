module Test.Ccap.Codegen.Util
  ( diffByLine
  , eqElems
  , onlyOne
  , parse
  , shouldBeLeft
  , shouldBeRight
--  , traceFile
  , validateModule
  ) where

import Prelude

import Ccap.Codegen.Module as Module
import Ccap.Codegen.Parser (errorMessage, parseSource)
import Ccap.Codegen.Types (Module, Source, ValidatedModule)
import Ccap.Codegen.ValidationError (joinErrors)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (sort)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), isLeft, isRight)
import Data.Foldable (traverse_)
import Data.List (List(..))
import Data.List as List
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Node.Path (FilePath)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

validateModule :: Source Module -> Effect (Either String (Source ValidatedModule))
validateModule = Array.singleton >>> Module.validateModules [] >>> map (joinErrors >=> onlyOne)

onlyOne :: forall a. Array a -> Either String a
onlyOne = List.fromFoldable >>> case _ of
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

--traceFile :: forall m. Monad m => String -> m Unit
--traceFile = traverse_ traceM <<< splitLines
