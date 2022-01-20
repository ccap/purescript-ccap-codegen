module Ccap.Codegen.Annotations
  ( getIsPositive
  , getMaxLength
  , getMaxSize
  , getMinLength
  , getWrapOpts
  , field
  ) where

import Prelude
import Ccap.Codegen.Cst as Cst
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Maybe (Maybe)

fieldWithOptParamValue :: String -> String -> Array Cst.Annotation -> Maybe (Maybe String)
fieldWithOptParamValue annotKey paramKey annots = do
  Cst.Annotation _ _ params <- Array.find (\(Cst.Annotation n _ _) -> n == annotKey) annots
  Cst.AnnotationParam _ _ v <- Array.find (\(Cst.AnnotationParam n _ _) -> n == paramKey) params
  pure v

field :: String -> String -> Array Cst.Annotation -> Maybe String
field annotKey paramKey = join <<< fieldWithOptParamValue annotKey paramKey

getWrapOpts :: String -> Array Cst.Annotation -> Maybe { typ :: String, decode :: String, encode :: String }
getWrapOpts lang an =
  let
    f n = field lang n an
  in
    do
      typ <- f "t"
      decode <- f "decode" <|> pure ""
      encode <- f "encode" <|> pure ""
      pure { typ, decode, encode }

getMaxLength :: Array Cst.Annotation -> Maybe String
getMaxLength = field "validations" "maxLength"

getMinLength :: Array Cst.Annotation -> Maybe String
getMinLength = field "validations" "minLength"

getMaxSize :: Array Cst.Annotation -> Maybe String
getMaxSize = field "validations" "maxSize"

getIsPositive :: Array Cst.Annotation -> Maybe Unit
getIsPositive annots = fieldWithOptParamValue "validations" "positive" annots $> unit
