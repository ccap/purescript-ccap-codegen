module Ccap.Codegen.Annotations
  ( getWrapOpts
  ) where

import Prelude
import Ccap.Codegen.Types (Annotation(..), Annotations, AnnotationParam(..))
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Maybe (Maybe)

field :: String -> String -> Annotations -> Maybe String
field annotKey paramKey annots = do
  Annotation _ _ params <- Array.find (\(Annotation n _ _) -> n == annotKey) annots
  AnnotationParam _ _ v <- Array.find (\(AnnotationParam n _ _) -> n == paramKey) params
  v

getWrapOpts :: String -> Annotations -> Maybe { typ :: String, wrap :: String, unwrap :: String }
getWrapOpts lang an =
  let f n = field lang n an
  in do
    typ <- f "t"
    wrap <- f "wrap" <|> pure ""
    unwrap <- f "unwrap" <|> pure ""
    pure { typ, wrap, unwrap }
