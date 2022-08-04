module Ccap.Codegen.Runtime
  ( Codec
  , JsonCodec
  , StandardDecoderApi
  , codec_custom
  , codec_newtype
  , composeCodec
  , decodeProperty
  , jsonCodec_array
  , jsonCodec_boolean
  , jsonCodec_constructor
  , jsonCodec_decimal
  , jsonCodec_int
  , jsonCodec_json
  , jsonCodec_maybe
  , jsonCodec_number
  , jsonCodec_string
  , obj
  , standardDecoderApi
  ) where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut
import Data.Bifunctor (lmap)
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Either (Either(..), note)
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Unsafe.Coerce as Unsafe

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

type Api
  = { isLeft :: forall a b. Either a b -> Boolean
    , fromRight :: forall a b. Partial => Either a b -> b
    , right :: forall a b. b -> Either a b
    , left :: forall a b. a -> Either a b
    }

type StandardDecoderApi
  = { nothing :: forall a. Maybe a
    , just :: forall a. a -> Maybe a
    , isLeft :: forall a b. Either a b -> Boolean
    , fromRight :: forall a b. Partial => Either a b -> b
    , right :: forall a b. b -> Either a b
    , left :: forall a b. a -> Either a b
    , addErrorPrefix :: forall a. String -> Either String a -> Either String a
    , jsonCodec_primitive_decimal :: JsonCodec Decimal
    }

standardDecoderApi :: StandardDecoderApi
standardDecoderApi =
  { nothing: Nothing
  , just: Just
  , isLeft: Either.isLeft
  , fromRight: Either.fromRight
  , right: Right
  , left: Left
  , addErrorPrefix: \s -> lmap (s <> _)
  , jsonCodec_primitive_decimal: jsonCodec_decimal
  }

api :: Api
api =
  { isLeft: Either.isLeft
  , fromRight: Either.fromRight
  , right: Right
  , left: Left
  }

foreign import decodeArray_ ::
  forall a.
  Api ->
  (Json -> Either String a) ->
  Json ->
  Either String (Array a)

foreign import decodeString_ ::
  Api ->
  Json ->
  Either String String

foreign import decodeBoolean_ ::
  Api ->
  Json ->
  Either String Boolean

foreign import decodeNumber_ ::
  Api ->
  Json ->
  Either String Number

foreign import decodeInt_ ::
  Api ->
  Json ->
  Either String Int

foreign import decodeObject_ ::
  Api ->
  Json ->
  Either String (Object Json)

foreign import lookup_ ::
  Api ->
  String ->
  Object Json ->
  Either String Json

foreign import isNull_ ::
  Json ->
  Boolean

jsonCodec_string :: JsonCodec String
jsonCodec_string =
  { decode: decodeString_ api
  , encode: Argonaut.fromString
  }

jsonCodec_decimal :: JsonCodec Decimal
jsonCodec_decimal =
  { decode:
      \j -> decodeString_ api j >>= (Decimal.fromString >>> note "This value must be a decimal")
  , encode: Argonaut.fromString <<< Decimal.toString
  }

jsonCodec_number :: JsonCodec Number
jsonCodec_number =
  { decode: decodeNumber_ api
  , encode: Argonaut.fromNumber
  }

jsonCodec_int :: JsonCodec Int
jsonCodec_int =
  { decode: decodeInt_ api
  , encode: Argonaut.fromNumber <<< Int.toNumber
  }

jsonCodec_boolean :: JsonCodec Boolean
jsonCodec_boolean =
  { decode: decodeBoolean_ api
  , encode: Argonaut.fromBoolean
  }

nothingResult :: forall a b. Either a (Maybe b)
nothingResult = Right Nothing

jsonCodec_maybe :: forall a. JsonCodec a -> JsonCodec (Maybe a)
jsonCodec_maybe w =
  { decode: \j -> if isNull_ j then nothingResult else map Just (w.decode j)
  , encode: \a -> maybe Argonaut.jsonNull w.encode a
  }

jsonCodec_array ::
  forall a.
  JsonCodec a ->
  JsonCodec (Array a)
jsonCodec_array inner =
  { decode: decodeArray_ api inner.decode
  , encode: Argonaut.fromArray <<< map inner.encode
  }

decodeProperty :: forall a. String -> JsonCodec a -> Object Json -> Either String a
decodeProperty prop codec o = do
  v <- lookup_ api prop o
  lmap (\s -> "Property " <> prop <> ": " <> s) (codec.decode v)

composeCodec ::
  forall a b c.
  Codec b c ->
  Codec a b ->
  Codec a c
composeCodec codec1 codec2 =
  { decode: \j -> codec2.decode j >>= codec1.decode
  , encode: codec1.encode >>> codec2.encode
  }

obj :: Json -> Either String (Object Json)
obj = decodeObject_ api

codec_custom ::
  forall t a b.
  (b -> Either String t) ->
  (t -> b) ->
  Codec a b ->
  Codec a t
codec_custom decode encode = composeCodec { decode, encode }

codec_newtype ::
  forall t a b.
  Newtype t b =>
  Codec a b ->
  Codec a t
codec_newtype = Unsafe.unsafeCoerce

jsonCodec_constructor :: JsonCodec (Tuple String (Array Json))
jsonCodec_constructor =
  { encode:
      \(Tuple name params) ->
        Argonaut.fromObject
          ( Object.fromFoldable
              [ Tuple "c" (Argonaut.fromString name)
              , Tuple "p" (Argonaut.fromArray params)
              ]
          )
  , decode:
      \j -> do
        o <- obj j
        c <- decodeProperty "c" jsonCodec_string o
        p <- decodeProperty "p" (jsonCodec_array jsonCodec_json) o
        pure (Tuple c p)
  }
