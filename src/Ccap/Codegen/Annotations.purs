module Ccap.Codegen.Annotations
  ( getInstances
  , getIsDbManaged
  , getIsPositive
  , getIsPrimaryKey
  , getMaxLength
  , getMaxSize
  , getMinLength
  , getPSInstances
  , getWrapOpts
  , field
  ) where

import Prelude
import Ccap.Codegen.Cst as Cst
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe (isJust)

params :: Array Cst.Annotation -> String -> Maybe (Array Cst.AnnotationParam)
params annots annotKey = do
  Cst.Annotation _ _ params' <-
    Array.find
      (\(Cst.Annotation n _ _) -> n == annotKey)
      annots
  pure params'

optParamValue :: Array Cst.AnnotationParam -> String -> Maybe (Maybe String)
optParamValue params' paramKey = do
  Cst.AnnotationParam _ _ v <-
    Array.find
      (\(Cst.AnnotationParam n _ _) -> n == paramKey)
      params'
  pure v

fieldWithOptParamValue :: String -> String -> Array Cst.Annotation -> Maybe (Maybe String)
fieldWithOptParamValue annotKey paramKey annots = do
  params' <- params annots annotKey
  optParamValue params' paramKey

field :: String -> String -> Array Cst.Annotation -> Maybe String
field annotKey paramKey = join <<< fieldWithOptParamValue annotKey paramKey

type WrapOpts
  = { typ :: String
    , decode :: String
    , empty :: Maybe String
    , encode :: String
    }


getWrapOpts :: String -> Array Cst.Annotation -> Maybe WrapOpts
getWrapOpts lang an =
  let
    f n = field lang n an
  in
    do
      typ <- f "t"
      decode <- f "decode" <|> pure ""
      empty <- Just $ f "empty"
      encode <- f "encode" <|> pure ""
      pure { typ, decode, empty, encode }

getMaxLength :: Array Cst.Annotation -> Maybe String
getMaxLength = field "validations" "maxLength"

getMinLength :: Array Cst.Annotation -> Maybe String
getMinLength = field "validations" "minLength"

getMaxSize :: Array Cst.Annotation -> Maybe String
getMaxSize = field "validations" "maxSize"

getPSInstances :: Array Cst.Annotation -> Maybe { generic :: Boolean }
getPSInstances annots = do
    params' <- params annots "psinstances"
    let
        generic = isJust $ optParamValue params' "generic"
    pure { generic }

getInstances :: Array Cst.Annotation -> Maybe { equal :: String, meta :: Maybe String }
getInstances annots = do
  params' <- params annots "instances"
  let
    attr = join <<< optParamValue params'
  equal <- attr "equal"
  pure { equal, meta: attr "meta" }

getIsDbManaged :: Array Cst.Annotation -> Boolean
getIsDbManaged = Array.any (\(Cst.Annotation name _ _) -> name == "dbManaged")

getIsPositive :: Array Cst.Annotation -> Maybe Unit
getIsPositive annots = fieldWithOptParamValue "validations" "positive" annots $> unit

getIsPrimaryKey :: Array Cst.Annotation -> Boolean
getIsPrimaryKey = Array.any (\(Cst.Annotation name _ _) -> name == "primaryKey")
