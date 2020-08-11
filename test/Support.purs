module Test.Support where

import Data.Newtype

newtype Newint
  = Newint Int

derive instance newtypeNewint :: Newtype Newint _
