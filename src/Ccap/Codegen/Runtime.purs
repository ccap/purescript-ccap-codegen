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
  , jsonCodec_short
  , jsonCodec_string
  , obj
  , standardDecoderApi
  ) where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut
import Data.Argonaut (JsonDecodeError(..))
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
  = { decode :: a -> Either JsonDecodeError b
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
    , left :: forall a b. a -> Either a b
    , missingValue :: forall b. String -> Either JsonDecodeError b
    , right :: forall a b. b -> Either a b
    , typeMismatch :: forall b. String -> Either JsonDecodeError b
    }

type StandardDecoderApi
  = { nothing :: forall a. Maybe a
    , fromRight :: forall a b. Partial => Either a b -> b
    , isLeft :: forall a b. Either a b -> Boolean
    , just :: forall a. a -> Maybe a
    , left :: forall a b. a -> Either a b
    , right :: forall a b. b -> Either a b
    , addErrorPrefix :: forall a. String -> Either JsonDecodeError a -> Either JsonDecodeError a
    , missingValue :: forall b. String -> Either JsonDecodeError b
    , typeMismatch :: forall b. String -> Either JsonDecodeError b
    , jsonCodec_primitive_decimal :: JsonCodec Decimal
    }

standardDecoderApi :: StandardDecoderApi
standardDecoderApi =
  { nothing: Nothing
  , just: Just
  , isLeft: Either.isLeft
  , fromRight: \(Either.Right v) -> v
  , right: Right
  , left: Left
  , addErrorPrefix: \name -> lmap (Named name)
  , jsonCodec_primitive_decimal: jsonCodec_decimal
  , missingValue: \name -> Left $ Named name MissingValue
  , typeMismatch: Left <<< TypeMismatch
  }

api :: Api
api =
  { isLeft: Either.isLeft
  , fromRight: \(Either.Right v) -> v
  , right: Right
  , left: Left
  , missingValue: \name -> Left $ Named name MissingValue
  , typeMismatch: Left <<< TypeMismatch
  }

foreign import decodeArray_ ::
  forall a.
  Api ->
  (Json -> Either JsonDecodeError a) ->
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
  { decode: lmap TypeMismatch <<< decodeString_ api
  , encode: Argonaut.fromString
  }

jsonCodec_decimal :: JsonCodec Decimal
jsonCodec_decimal =
  { decode:
      \j -> (lmap TypeMismatch $ decodeString_ api j) >>= (Decimal.fromString >>> note (TypeMismatch "This value must be a decimal"))
  , encode: Argonaut.fromString <<< Decimal.toString
  }

jsonCodec_number :: JsonCodec Number
jsonCodec_number =
  { decode: lmap TypeMismatch <<< decodeNumber_ api
  , encode: Argonaut.fromNumber
  }

-- TODO (DRS): Fix in codegen
jsonCodec_short :: JsonCodec Int
jsonCodec_short = jsonCodec_int

jsonCodec_int :: JsonCodec Int
jsonCodec_int =
  { decode: lmap TypeMismatch <<< decodeInt_ api
  , encode: Argonaut.fromNumber <<< Int.toNumber
  }

jsonCodec_boolean :: JsonCodec Boolean
jsonCodec_boolean =
  { decode: lmap TypeMismatch <<< decodeBoolean_ api
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
  { decode: lmap TypeMismatch <<< decodeArray_ api inner.decode
  , encode: Argonaut.fromArray <<< map inner.encode
  }

decodeProperty :: forall a. String -> JsonCodec a -> Object Json -> Either JsonDecodeError a
decodeProperty prop codec o = do
  v <- lmap TypeMismatch $ lookup_ api prop o
  lmap (\s -> TypeMismatch $ "Property " <> prop <> ": " <> show s) (codec.decode v)

composeCodec ::
  forall a b c.
  Codec b c ->
  Codec a b ->
  Codec a c
composeCodec codec1 codec2 =
  { decode: \j -> codec2.decode j >>= codec1.decode
  , encode: codec1.encode >>> codec2.encode
  }

obj :: Json -> Either JsonDecodeError (Object Json)
obj = lmap TypeMismatch <<< decodeObject_ api

codec_custom ::
  forall t a b.
  (b -> Either JsonDecodeError t) ->
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
