module Ccap.Codegen.Annotations
  ( getWrapOpts
  ) where

import Prelude
import Ccap.Codegen.Types (Annotation(..), Annotations, AnnotationParam(..))
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Maybe (Maybe(..))

arrayMap :: forall a k v. Eq k => (a -> k) -> (a -> v) -> Array a -> k -> Maybe v
arrayMap k v arr i =
  Array.findMap (\e -> if k e == i then Just $ v e else Nothing) arr

lookupAnnotation :: String -> Annotations -> Maybe (Array AnnotationParam)
lookupAnnotation =
  arrayMap (\(Annotation l _ _) -> l) (\(Annotation _ _ p) -> p) # flip

lookupParam :: String -> Array AnnotationParam -> Maybe (Maybe String)
lookupParam =
  arrayMap (\(AnnotationParam k _ _) -> k) (\(AnnotationParam _ _ v) -> v) # flip

field :: String -> String -> Annotations -> Maybe (Maybe String)
field s f = lookupAnnotation s >=> lookupParam f

getWrapOpts :: String -> Annotations -> Maybe { typ :: String, wrap :: String, unwrap :: String }
getWrapOpts lang an =
  let f n = field lang n an # join
  in do
    typ <- f "t"
    wrap <- f "wrap" <|> pure ""
    unwrap <- f "unwrap" <|> pure ""
    pure { typ, wrap, unwrap }
