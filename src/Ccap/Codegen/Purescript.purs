module Ccap.Codegen.Purescript
  ( outputSpec
  ) where

import Prelude

import Ccap.Codegen.Annotations (getWrapOpts)
import Ccap.Codegen.Shared (DelimitedLiteralDir(..), Emit, OutputSpec, delimitedLiteral, emit, indented)
import Ccap.Codegen.Types (Import(..), Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Control.Monad.Writer (runWriter, tell)
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Text.PrettyPrint.Boxes (Box, char, hsep, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (bottom, left) as Boxes

oneModule :: String -> Module -> Box
oneModule module_ (Module name imps decls) = vsep 1 Boxes.left do
  let mimps = imps <#> importModule module_
  let Tuple result imports = runWriter (traverse typeDecl decls)
      is = (mimps <> imports) # Array.sort >>> Array.nub
  text ("module " <> module_ <> "." <> name <> " where")
    : vcat Boxes.left (is <#> \i -> text ("import " <> i))
    : result

outputSpec :: String -> OutputSpec
outputSpec package =
  { render: render <<< oneModule package
  , fileName: \(Module n _ _) -> n <> ".purs"
  }

primitive :: Primitive -> Emit Box
primitive = case _ of
  PBoolean -> pure (text "Boolean")
  PInt -> pure (text "Int")
  PDecimal -> emit [ "Data.Decimal (Decimal)" ] (text "Decimal")
  PString -> pure (text "String")

type Extern = { prefix :: String, t :: String}

externalType :: Extern -> Emit Box
externalType { prefix, t } =
  emit [ prefix <> " (" <> t <> ")" ] $ text t

importModule :: String -> Import -> String
importModule package (Import mod) =
  package <> "." <> mod <> " as " <> mod

splitType :: String -> Maybe Extern
splitType s = do
  i <- String.lastIndexOf (Pattern ".") s
  let prefix = String.take i s
  let t = String.drop (i + 1) s
  pure $ { prefix, t }

typeDecl :: TypeDecl -> Emit Box
typeDecl (TypeDecl name tt annots) =
  let dec kw = text kw <<+>> text name <<+>> char '='
  in case tt of
    Type t -> do
      ty <- tyType t
      j <- jsonCodec t
      pure $ (dec "type" <<+>> ty)
        // defJsonCodec name j
    Wrap t ->
      case getWrapOpts "purs" annots of
        Nothing -> do
          other <- otherInstances name
          ty <- tyType t
          j <- newtypeJsonCodec name t
          newtype_ <- newtypeInstances name
          pure $
            dec "newtype" <<+>> text name <<+>> ty
              // newtype_
              // other
              // defJsonCodec name j
        Just { typ, decode, encode } -> do
          ty <- externalRef typ
          j <- externalJsonCodec name t decode encode
          pure $
            dec "type" <<+>> ty
              // defJsonCodec name j
    Record props -> do
      recordDecl <- record props <#> \p -> dec "type" // indented p
      codec <- recordJsonCodec props
      pure $
        recordDecl
          // defJsonCodec name codec
    Sum vs -> do
      other <- otherInstances name
      codec <- sumJsonCodec name vs
      pure $
        dec "data"
          // indented (hsep 1 Boxes.bottom $ vcat Boxes.left <$> [ Array.drop 1 vs <#> \_ -> char '|',  vs <#> text ])
          // other
          // defJsonCodec name codec

sumJsonCodec :: String -> Array String -> Emit Box
sumJsonCodec name vs = do
  tell
    [ "Data.Either (Either(..))"
    ]
  let encode = text "encode: case _ of"
                // indented (branches encodeBranch)
      decode = text "decode: case _ of"
                // indented (branches decodeBranch // fallthrough)
  pure $ text "Runtime.composeCodec"
          // indented (delimitedLiteral Vert '{' '}' [ decode, encode ] // text "Runtime.jsonCodec_string")
  where
    branches branch = vcat Boxes.left (vs <#> branch)
    encodeBranch v = text v <<+>> text "->" <<+>> text (show v)
    decodeBranch v = text (show v) <<+>> text "-> Right" <<+>> text v
    fallthrough = text $ "s -> Left $ \"Invalid value \" <> show s <> \" for " <> name <> "\""

newtypeInstances :: String -> Emit Box
newtypeInstances name =
  emit
    [ "Data.Newtype (class Newtype)" ]
    $ text ("derive instance newtype" <> name <> " :: Newtype " <> name <> " _")

otherInstances :: String -> Emit Box
otherInstances name =
  emit
    [ "Prelude"
    , "Data.Generic.Rep (class Generic)"
    , "Data.Generic.Rep.Show (genericShow)"
    ]
    $ text ("derive instance eq" <> name <> " :: Eq " <> name)
        // text ("derive instance ord" <> name <> " :: Ord " <> name)
        // text ("derive instance generic" <> name <> " :: Generic " <> name <> " _")
        // text ("instance show" <> name <> " :: Show " <> name <> " where")
        // indented (text "show a = genericShow a")

tyType :: Type -> Emit Box
tyType =
  let
    wrap tycon t =
      tyType t <#> \ty -> text tycon <<+>> parens ty
  in case _ of
    Primitive p -> primitive p
    Ref _ { mod, typ } -> pure $ text (maybe "" (_ <> ".") mod <> typ)
    Array t -> wrap "Array" t
    Option t -> tell (pure "Data.Maybe (Maybe)") >>= const (wrap "Maybe" t)

externalRef :: String -> Emit Box
externalRef s = fromMaybe (text s # pure) (splitType s <#> externalType)

emitRuntime :: Box -> Emit Box
emitRuntime b = emit [ "Ccap.Codegen.Runtime as Runtime" ] b

newtypeJsonCodec :: String -> Type -> Emit Box
newtypeJsonCodec name t = do
  i <- jsonCodec t
  emitRuntime $ text "Runtime.codec_newtype" <<+>> parens i

externalJsonCodec :: String -> Type -> String -> String -> Emit Box
externalJsonCodec name t decode encode = do
  i <- jsonCodec t
  decode_ <- externalRef decode
  encode_ <- externalRef encode
  emitRuntime $ text "Runtime.custom_codec" <<+>> decode_ <<+>> encode_ <<+>> parens i

codecName :: Maybe String -> String -> String
codecName mod t =
  maybe "" (_ <> ".") mod <> "jsonCodec_" <> t

jsonCodecS :: Type -> String
jsonCodecS ty =
  case ty of
    Primitive p -> codecName (Just "Runtime") (
      case p of
        PBoolean -> "boolean"
        PInt -> "int"
        PDecimal -> "decimal"
        PString -> "string"
      )
    Array t -> tycon "array" t
    Option t -> tycon "maybe" t
    Ref _ { mod, typ } -> codecName mod typ
  where
    tycon which t =
      "(" <> codecName (Just "Runtime") which <> " " <> jsonCodecS t <> ")"

jsonCodec :: Type -> Emit Box
jsonCodec = jsonCodecS >>> text >>> pure

parens :: Box -> Box
parens b = char '(' <<>> b <<>> char ')'

defJsonCodec :: String -> Box -> Box
defJsonCodec name def =
  let cname = codecName Nothing name
  in text cname <<+>> text ":: Runtime.JsonCodec" <<+>> text name
     // (text cname <<+>> char '=')
     // indented def

record :: Array RecordProp -> Emit Box
record props = do
  tell [ "Data.Tuple (Tuple(..))" ]
  types <- (\(RecordProp _ t) -> tyType t) `traverse` props
  let labels = props <#> \(RecordProp name _) -> text name <<+>> text "::"
  pure $ delimitedLiteral Vert '{' '}' (Array.zip labels types <#> \(Tuple l t) -> l <<+>> t)

recordJsonCodec :: Array RecordProp -> Emit Box
recordJsonCodec props = do
  tell
    [ "Data.Argonaut.Core as Argonaut"
    , "Ccap.Codegen.Runtime as Runtime"
    , "Foreign.Object as FO"
    , "Prelude"
    ]
  encodeProps <- recordWriteProps props
  decodeProps <- recordReadProps props
  let encode = text "encode: \\p -> Argonaut.fromObject $"
                // indented
                      (text "FO.fromFoldable"
                        // indented encodeProps)
      names = props <#> \(RecordProp name _) -> text name
      decode = text "decode: \\j -> do"
              // indented
                    (text "o <- Runtime.obj j"
                      // decodeProps
                      // (text "pure" <<+>> delimitedLiteral Horiz '{' '}' names))
  pure $ delimitedLiteral Vert '{' '}' [ decode, encode ]

recordWriteProps :: Array RecordProp -> Emit Box
recordWriteProps props = do
  types <- for props (\(RecordProp name t) -> do
    x <- jsonCodec t
    pure $ text "Tuple" <<+>> text (show name) <<+>> parens (x <<>> text ".encode p." <<>> text name))
  pure $ delimitedLiteral Vert '[' ']' types

recordReadProps :: Array RecordProp -> Emit Box
recordReadProps props = do
  lines <- for props (\(RecordProp name t) -> do
    x <- jsonCodec t
    pure $ text name <<+>> text "<- Runtime.decodeProperty"
              <<+>> text (show name) <<+>> x <<+>> text "o")
  pure $ vcat Boxes.left lines
