module Ccap.Codegen.Runtime where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut
import Data.Bifunctor (lmap)
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object (Object) as FO
import Foreign.Object (lookup) as Object

type Codec a b
  = { decode :: a -> Either String b
    , encode :: b -> a
    }

type JsonCodec a
  = Codec Json a

jsonCodec_json :: JsonCodec Json
jsonCodec_json =
  { decode: Right
  , encode: identity
  }

jsonCodec_string :: JsonCodec String
jsonCodec_string =
  { decode: maybe (Left "This value must be a string") Right <<< Argonaut.toString
  , encode: Argonaut.fromString
  }

jsonCodec_decimal :: JsonCodec Decimal
jsonCodec_decimal =
  composeCodec
    { decode: maybe (Left "This value must be a decimal") Right <<< Decimal.fromString
    , encode: Decimal.toString
    }
    jsonCodec_string

jsonCodec_number :: JsonCodec Number
jsonCodec_number =
  { decode: maybe (Left "This value must be a number") Right <<< Argonaut.toNumber
  , encode: Argonaut.fromNumber
  }

jsonCodec_int :: JsonCodec Int
jsonCodec_int =
  composeCodec
    { decode: maybe (Left "This value must be an integer") Right <<< Int.fromNumber
    , encode: Int.toNumber
    }
    jsonCodec_number

jsonCodec_boolean :: JsonCodec Boolean
jsonCodec_boolean =
  { decode: maybe (Left "This value must be a boolean") Right <<< Argonaut.toBoolean
  , encode: Argonaut.fromBoolean
  }

jsonCodec_maybe :: forall a. JsonCodec a -> JsonCodec (Maybe a)
jsonCodec_maybe w =
  { decode: \j -> if Argonaut.isNull j then Right Nothing else map Just (w.decode j)
  , encode: \a -> maybe Argonaut.jsonNull w.encode a
  }

jsonCodec_array ::
  forall a.
  JsonCodec a ->
  JsonCodec (Array a)
jsonCodec_array inner =
  { decode: maybe (Left "This value must be an array") (traverse inner.decode) <<< Argonaut.toArray
  , encode: Argonaut.fromArray <<< (map inner.encode)
  }

decodeProperty :: forall a. String -> JsonCodec a -> FO.Object Json -> Either String a
decodeProperty prop codec o = do
  v <-
    maybe
      (Left $ "Property " <> prop <> " does not exist")
      Right
      (Object.lookup prop o)
  lmap (\s -> "Property " <> prop <> ": " <> s) (codec.decode v)

composeCodec ::
  forall a b c.
  Codec b c ->
  Codec a b ->
  Codec a c
composeCodec codec1 codec2 =
  { decode: map (flip bind codec1.decode) codec2.decode
  , encode: codec1.encode >>> codec2.encode
  }

obj :: Json -> Either String (Object Json)
obj = maybe (Left "This value must be an object") Right <<< Argonaut.toObject

codec_custom ::
  forall t a b.
  (b -> Either String t) ->
  (t -> b) ->
  Codec a b ->
  Codec a t
codec_custom decode encode = composeCodec { decode, encode }

decodeNewtype :: forall t a. Newtype t a => a -> Either String t
decodeNewtype a = Right $ wrap a

codec_newtype ::
  forall t a b.
  Newtype t b =>
  Codec a b ->
  Codec a t
codec_newtype =
  composeCodec
    { decode: decodeNewtype
    , encode: unwrap
    }
