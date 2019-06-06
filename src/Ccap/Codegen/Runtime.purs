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

type Codec a b =
  { read :: a -> Either String b
  , write :: b -> a
  }

type JsonCodec a = Codec Json a

jsonCodec_string :: JsonCodec String
jsonCodec_string =
  { read: maybe (Left "This value must be a string") Right <<< Argonaut.toString
  , write: Argonaut.fromString
  }

jsonCodec_decimal :: JsonCodec Decimal
jsonCodec_decimal = composeCodec
  { read: maybe (Left "This value must be a decimal") Right <<< Decimal.fromString
  , write: Decimal.toString
  }
  jsonCodec_string

jsonCodec_number :: JsonCodec Number
jsonCodec_number =
  { read: maybe (Left "This value must be a number") Right <<< Argonaut.toNumber
  , write: Argonaut.fromNumber
  }

jsonCodec_int :: JsonCodec Int
jsonCodec_int = composeCodec
  { read: maybe (Left "This value must be an integer") Right <<< Int.fromNumber
  , write: Int.toNumber
  }
  jsonCodec_number

jsonCodec_boolean :: JsonCodec Boolean
jsonCodec_boolean =
  { read: maybe (Left "This value must be a boolean") Right <<< Argonaut.toBoolean
  , write: Argonaut.fromBoolean
  }

jsonCodec_maybe :: forall a. JsonCodec a -> JsonCodec (Maybe a)
jsonCodec_maybe w =
  { read: \j -> if Argonaut.isNull j then Right Nothing else map Just (w.read j)
  , write: \a -> maybe Argonaut.jsonNull w.write a
  }

jsonCodec_array
  :: forall a
   . JsonCodec a
  -> JsonCodec (Array a)
jsonCodec_array inner =
  { read: maybe (Left "This value must be an array") (traverse inner.read) <<< Argonaut.toArray
  , write: Argonaut.fromArray <<< (map inner.write)
  }

readProperty :: forall a. String -> JsonCodec a -> FO.Object Json -> Either String a
readProperty prop codec o = do
  v <- maybe
        (Left $ "Property " <> prop <> " does not exist")
        Right
        (Object.lookup prop o)
  lmap (\s -> "Property " <> prop <> ": " <> s) (codec.read v)

composeCodec
  :: forall a b c
   . Codec b c
  -> Codec a b
  -> Codec a c
composeCodec codec1 codec2 =
  { read: map (flip bind codec1.read) codec2.read
  , write: codec1.write >>> codec2.write
  }

obj :: Json -> Either String (Object Json)
obj = maybe (Left "This value must be an object") Right <<< Argonaut.toObject

codec_custom
  :: forall t a b
   . (b -> Either String t)
  -> (t -> b)
  -> Codec a b
  -> Codec a t
codec_custom read write = composeCodec { read, write }

codec_newtype
  :: forall t a b
   . Newtype t b
  => Codec a b
  -> Codec a t
codec_newtype = composeCodec
  { read: Right <<< wrap
  , write: unwrap
  }
