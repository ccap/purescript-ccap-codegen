module Ccap.Codegen.Purescript
  ( outputSpec
  , prettyPrint
  ) where

import Prelude

import Ccap.Codegen.Shared (OutputSpec, indented)
import Ccap.Codegen.Types (Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Data.Array (drop, length, snoc, (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hsep, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (bottom, left) as Boxes

prettyPrint :: String -> Array Module -> String
prettyPrint module_ modules =
  render $ vsep 1 Boxes.left (modules <#> oneModule module_)

oneModule :: String -> Module -> Box
oneModule module_ (Module name decls) = vsep 1 Boxes.left do
  text ("module " <> module_ <> "." <> name <> " where")
    : text "import Data.Maybe (Maybe)"
    : (decls <#> typeDecl true)

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

typeDecl :: Boolean -> TypeDecl -> Box
typeDecl last (TypeDecl name tt _) =
  let dec kw = text kw <<+>> text name <<+>> char '='
  in case tt of
    Type t ->
      dec "type" <<+>> tyType t
    Wrap t wo ->
      case Map.lookup "scala" wo of
        Nothing ->
          dec "newtype" <<+>> text name <<+>> tyType t
            // newtypeInstances name
            // otherInstances name
        Just { typ, wrap, unwrap } ->
          dec "type" <<+>> text typ
            // text "-- TODO: Emit import for above type when needed"
    Record props ->
      dec "type" // indented (record props)
    Sum vs ->
      dec "data"
        // indented (hsep 1 Boxes.bottom $ vcat Boxes.left <$> [ drop 1 vs <#> \_ -> char '|',  vs <#> text ])
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

tyType :: Type -> Box
tyType =
  let wrap tycon t = text tycon <<+>> char '(' <<>> tyType t <<>> char ')'
  in case _ of
  Primitive p -> primitive p
  Ref _ s -> text s
  Array t -> wrap "Array" t
  Option t ->  wrap "Maybe" t

record :: Array RecordProp -> Box
record props =
  let
    len = length props
    space = emptyBox 0 1
    columns =
      [ snoc ('{' : (drop 1 props <#> const ',')) '}' <#> char
      , props <#> \(RecordProp name _) -> text name
      , props <#> const (text "::")
      , props <#> \(RecordProp _ t) -> tyType t
      ] <#> vcat Boxes.left
  in hsep 1 Boxes.left columns
