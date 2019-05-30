module Ccap.Codegen.Purescript
  ( outputSpec
  , prettyPrint
  ) where

import Prelude

import Ccap.Codegen.Shared (OutputSpec, indented)
import Ccap.Codegen.Types (Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array ((:))
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hsep, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (bottom, left) as Boxes

type Imports = Array String

type Emit = Writer Imports

emit :: forall a. Array String -> a -> Emit a
emit imports a = map (const a) (tell imports)

prettyPrint :: String -> Array Module -> String
prettyPrint module_ modules =
  render $ vsep 1 Boxes.left (modules <#> oneModule module_)

oneModule :: String -> Module -> Box
oneModule module_ (Module name decls) = vsep 1 Boxes.left do
  let Tuple result imports = runWriter (traverse typeDecl decls)
      is = imports # Array.sort >>> Array.nub
  text ("module " <> module_ <> "." <> name <> " where")
    : vcat Boxes.left (is <#> \i -> text ("import " <> i))
    : result

outputSpec :: String -> OutputSpec
outputSpec package =
  { render: render <<< oneModule package
  , fileName: \(Module n _) -> n <> ".purs"
  }

primitive :: Primitive -> Box
primitive p = text
  case p of
    PBoolean -> "Boolean"
    PInt -> "Int"
    PDecimal -> "Number" -- ish
    PString -> "String"

type Extern = { prefix :: String, t :: String}

externalType :: Extern -> Emit Box
externalType { prefix, t } =
  emit [ prefix <> " (" <> t <> ")" ] $ text t

splitType :: String -> Maybe Extern
splitType s = do
  i <- String.lastIndexOf (Pattern ".") s
  let prefix = String.take i s
  let t = String.drop (i + 1) s
  pure $ { prefix, t }

typeDecl :: TypeDecl -> Emit Box
typeDecl (TypeDecl name tt _) =
  let dec kw = text kw <<+>> text name <<+>> char '='
  in case tt of
    Type t ->
      tyType t <#> (dec "type" <<+>> _)
    Wrap t wo ->
      case Map.lookup "purs" wo of
        Nothing ->
          tyType t <#> (\ty ->
            dec "newtype" <<+>> text name <<+>> ty
              // newtypeInstances name
              // otherInstances name)
        Just { typ, wrap, unwrap } ->
          fromMaybe { prefix: "?missingPrefix", t: typ } (splitType typ)
            # externalType <#> (dec "type" <<+>> _)
    Record props ->
      record props <#> \p -> dec "type" // indented p
    Sum vs -> pure do
      dec "data"
        // indented (hsep 1 Boxes.bottom $ vcat Boxes.left <$> [ Array.drop 1 vs <#> \_ -> char '|',  vs <#> text ])
        // otherInstances name
        // encodeJsonSum name vs
        // decodeJsonSum name vs

encodeJsonSum :: String -> Array String -> Box
encodeJsonSum name vs =
  text ("instance encodeJson" <> name <> " :: EncodeJson " <> name <> " where")
    // indented (text "encodeJson s = encodeJson $ case s of" // indented branches)
  where
    branches = vcat Boxes.left (vs <#> branch)
    branch v = text v <<+>> text "->" <<+>> text (show v)

decodeJsonSum :: String -> Array String -> Box
decodeJsonSum name vs =
  text ("instance decodeJson" <> name <> " :: DecodeJson " <> name <> " where")
    // indented (text "decodeJson j = do" // indented (decodeString // decodeBranches))
  where
    decodeString = text "s <- decodeJson"
    decodeBranches =
      text "case s of"
        // indented (branches // fallthrough)
    branches = vcat Boxes.left (vs <#> branch)
    branch v = text (show v) <<+>> text "-> Right" <<+>> text v
    fallthrough = text $ "_ -> Left \"Invalid value \" <> show s <> \"for " <> name <> "\""

newtypeInstances :: String -> Box
newtypeInstances name =
  text ("derive instance newtype" <> name <> " :: Newtype " <> name <> " _")
    // text ("derive newtype instance decodeJson" <> name <> " :: DecodeJson " <> name <> " _")
    // text ("derive newtype instance encodeJson" <> name <> " :: EncodeJson " <> name <> " _")

otherInstances :: String -> Box
otherInstances name =
  text ("derive instance eq" <> name <> " :: Eq " <> name)
    // text ("derive instance ord" <> name <> " :: Ord " <> name)
    // text ("derive instance generic" <> name <> " :: Generic " <> name <> " _")
    // text ("instance show" <> name <> " :: Show " <> name <> " where")
    // indented (text "show a = genericShow a")

tyType :: Type -> Emit Box
tyType =
  let
    wrap tycon t =
      tyType t <#> \ty -> text tycon <<+>> char '(' <<>> ty <<>> char ')'
  in case _ of
    Primitive p -> pure $ primitive p
    Ref _ s -> fromMaybe (text s # pure) (splitType s <#> externalType)
    Array t -> wrap "Array" t
    Option t -> tell (pure "Data.Maybe (Maybe)") >>= const (wrap "Maybe" t)

record :: Array RecordProp -> Emit Box
record props = do
  let len = Array.length props
      space = emptyBox 0 1
  types <- (\(RecordProp _ t) -> tyType t) `traverse` props
  let labels = props <#> \(RecordProp name _) -> text name <<+>> text "::"
      columns =
        [ Array.snoc ('{' : (Array.drop 1 props <#> const ',')) '}' <#> char
        , Array.zip labels types <#> \(Tuple l t) -> l <<+>> t
        ] <#> vcat Boxes.left
  pure (hsep 1 Boxes.left columns)
