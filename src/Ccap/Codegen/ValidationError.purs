module Ccap.Codegen.ValidationError
  ( class ValidationError
  , joinErrors
  , printError
  , toValidation
  ) where

import Prelude
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (class Foldable, intercalate)
import Data.Traversable (traverse)

class ValidationError a where
  printError :: a -> String

-- | Map over an array of checks and collect either all errors or all results
toValidation :: forall a b. Array (Either a b) -> Either (Array a) (Array b)
toValidation = traverse $ lmap Array.singleton

joinErrors :: forall f a. Foldable f => Either (f String) a -> Either String a
joinErrors = lmap $ intercalate "\n"
