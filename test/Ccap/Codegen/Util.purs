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
  , sourceAstTmpl
  , sourceCstTmpl
  , splitLines
  ) where

import Prelude
import Ccap.Codegen.Ast as Ast
import Ccap.Codegen.AstBuilder as AstBuilder
import Ccap.Codegen.Cst as Cst
import Ccap.Codegen.Error as Error
import Ccap.Codegen.FileSystem (readTextFile)
import Ccap.Codegen.Parser as Parser2
import Ccap.Codegen.Shared (OutputSpec)
import Ccap.Codegen.Util (ensureNewline, scrubEolSpaces)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Data.Array (sort)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, isLeft, isRight, note)
import Data.Foldable (traverse_)
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy (find)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..))
import Data.String (joinWith, split) as String
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

onlyOne :: forall a. Array a -> Either String a
onlyOne =
  List.fromFoldable
    >>> case _ of
        Cons x Nil -> Right x
        Nil -> Left "Array is empty"
        Cons _ _ -> Left "Array has multiple elements"

shouldBeRight :: forall m a b. MonadThrow Error m => Show a => Show b => Either a b -> m Unit
shouldBeRight = flip shouldSatisfy isRight

shouldBeLeft :: forall m a b. MonadThrow Error m => Show a => Show b => Either a b -> m Unit
shouldBeLeft = flip shouldSatisfy isLeft

eqElems :: forall a. Ord a => Eq a => Array a -> Array a -> Boolean
eqElems xs ys = sort xs == sort ys

parse :: FilePath -> String -> Either String (Cst.Source Cst.Module)
parse filePath = lmap Parser2.errorMessage <<< Parser2.parseSource filePath

print :: OutputSpec -> Cst.Source Ast.Module -> String
print { render } { contents } = ensureNewline $ scrubEolSpaces $ fromMaybe "" $ render contents

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

sourceCstTmpl :: FilePath -> Effect (Either String (Cst.Source Cst.Module))
sourceCstTmpl filePath =
  runExceptT do
    text <- ExceptT $ readTextFile filePath
    except $ parse filePath text

sourceAstTmpl :: FilePath -> Effect (Either String (Cst.Source Ast.Module))
sourceAstTmpl filePath =
  runExceptT do
    result <-
      ExceptT
        ( map
            (lmap (String.joinWith "\n" <<< NonEmptyArray.toArray <<< map (\e -> "ERROR: " <> Error.toString e)))
            (runExceptT (AstBuilder.build { files: [ filePath ], importPaths: [] }))
        )
    except (note "Expected result" (Array.head result))

matchKeyLine :: FilePath -> String -> OutputSpec -> String -> Aff Unit
matchKeyLine file keyWord outSpec line =
  runOrFail do
    source <- exceptAffT $ sourceAstTmpl file
    let
      printed = print outSpec source

      keyLine =
        findLine (String.includes keyWord) printed
          # fromMaybe ("Keyword, " <> keyWord <> ", not found")
    pure $ keyLine `shouldEqual` line
