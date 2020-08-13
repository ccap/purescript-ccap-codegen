module Ccap.Codegen.Annotations
  ( getIsPositive
  , getMaxLength
  , getMaxSize
  , getMinLength
  , getWrapOpts
  , field
  ) where

import Prelude
import Ccap.Codegen.Types (Annotation(..), Annotations, AnnotationParam(..))
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Maybe (Maybe)

fieldWithOptParamValue :: String -> String -> Annotations -> Maybe (Maybe String)
fieldWithOptParamValue annotKey paramKey annots = do
  Annotation _ _ params <- Array.find (\(Annotation n _ _) -> n == annotKey) annots
  AnnotationParam _ _ v <- Array.find (\(AnnotationParam n _ _) -> n == paramKey) params
  pure v

field :: String -> String -> Annotations -> Maybe String
field annotKey paramKey = join <<< fieldWithOptParamValue annotKey paramKey

getWrapOpts :: String -> Annotations -> Maybe { typ :: String, decode :: String, encode :: String }
getWrapOpts lang an =
  let
    f n = field lang n an
  in
    do
      typ <- f "t"
      decode <- f "decode" <|> pure ""
      encode <- f "encode" <|> pure ""
      pure { typ, decode, encode }

getMaxLength :: Annotations -> Maybe String
getMaxLength = field "validations" "maxLength"

getMinLength :: Annotations -> Maybe String
getMinLength = field "validations" "minLength"

getMaxSize :: Annotations -> Maybe String
getMaxSize = field "validations" "maxSize"

getIsPositive :: Annotations -> Maybe Unit
getIsPositive annots = fieldWithOptParamValue "validations" "positive" annots $> unit
