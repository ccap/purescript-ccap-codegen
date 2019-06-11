module Ccap.Codegen.Scala
  ( outputSpec
  , prettyPrint
  ) where

import Prelude

import Ccap.Codegen.Annotations (getWrapOpts)
import Ccap.Codegen.Shared (OutputSpec, indented)
import Ccap.Codegen.Types (Import(..), Imports, Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Text.PrettyPrint.Boxes (Box, char, hcat, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (left, top) as Boxes

prettyPrint :: String -> Array Module -> String
prettyPrint package modules =
  render $ vsep 1 Boxes.left (modules <#> oneModule package)

outputSpec :: String -> OutputSpec
outputSpec package =
  { render: render <<< oneModule package
  , fileName: \(Module n _ _) -> n <> ".scala"
  }

oneModule :: String -> Module -> Box
oneModule package (Module name imps decls) = vsep 1 Boxes.left do
  text ("package " <> package)
    : imports package imps
    : text ("object " <> name <> " {")
    : (decls <#> typeDecl true >>> indented)
    `Array.snoc` text "}"

curly :: Box -> Array Box -> Box
curly pref inner =
  vcat Boxes.left (pref <<+>> char '{' : (indented <$> inner) `Array.snoc` char '}')

paren :: Box -> Array Box -> Box
paren pref inner =
  vcat Boxes.left (pref <<>> char '(' : (indented <$> inner) `Array.snoc` char ')')

-- TODO: Clean up when we switch to a proper pretty printer.
-- Like `paren`, but outputs on a sigle line.
paren1 :: Box -> Array Box -> Box
paren1 pref inner =
  hcat Boxes.top (pref <<>> char '(' : inner `Array.snoc` char ')')

imports :: String -> Imports -> Box
imports package imps = vcat Boxes.left do
  Import mod <- imps
  -- TODO: Check for alternate package name in annotation. (if we need to import
  --       modules from other packages)
  pure $ text ("import " <> package <> "." <> mod)

defEncoder :: String -> Box -> Box
defEncoder name fun =
  text ("implicit def encodeJson" <> name <> ": argonaut.EncodeJson[" <> name <> "] =")
    // indented (text "argonaut.EncodeJson" `paren` [ fun ])

wrapEncoder :: String -> Type -> String -> Box
wrapEncoder name t unwrap = defEncoder name do
  text "x =>" <<+>> encodeType t (text (unwrap <> "(x)"))

typeDecl :: Boolean -> TypeDecl -> Box
typeDecl last (TypeDecl name tt an) =
  case tt of
    Type t ->
      text "type" <<+>> text name <<+>> char '=' <<+>> tyType t
      // defEncoder name (text "x => " <<>> encodeType t (text "x"))
    Wrap t ->
      case getWrapOpts "scala" an of
        Nothing ->
          let
            tagname = text (name <> "T")
            scalatyp = text"scalaz.@@[" <<>> tyType t <<>> char ',' <<+>> tagname <<>> char ']'
          in vcat Boxes.left
            [ text "final abstract class" <<+>> tagname
            , text "type" <<+>> text name <<+>> char '=' <<+>> scalatyp
            , wrapEncoder name t "scalaz.Tag.unwrap"
            ]
        Just { typ, wrap, unwrap } ->
          text "type" <<+>> text name <<+>> char '=' <<+>> text typ
          // wrapEncoder name t unwrap
    Record props ->
      let
        cls = (text "final case class" <<+>> text name) `paren` (recordFieldType <$> props)

        enc = defEncoder name (text "x => argonaut.Json.obj" `paren` (recordFieldEncoder <$> props))
      in cls // enc
    Sum vs ->
      let
        trait = (text "sealed trait" <<+>> text name) `curly` [ text "def tag: String"]
        variants = vs <#> \v ->
          text ("case object " <> v <> " extends " <> name)
            `curly` [ text ("override def tag: String = \"" <> v <> "\"")]
        encoder = defEncoder name (text "x => argonaut.Argonaut.jString(x.tag)")
      in
       trait // ((text "object" <<+>> text name) `curly` variants ) // encoder

tyType :: Type -> Box
tyType =
  let wrap tycon t = text tycon <<>> char '[' <<>> tyType t <<>> char ']'
  in case _ of
    Ref _ { mod, typ } -> text (maybe "" (_ <> ".") mod <> typ)
    Array t -> wrap "List" t
    Option t ->  wrap "Option" t
    Primitive p -> text (case p of
      PBoolean -> "Boolean"
      PInt -> "Int"
      PDecimal -> "BigDecimal"
      PString -> "String"
    )

encoderType :: Type -> Box
encoderType =
  let call fun e = text fun `paren1` [ e ]
  in case _ of
    Ref _ { mod, typ } -> text (maybe "" (_ <> ".") mod <> "encodeJson" <> typ)
    Array t -> call "argonaut.EncodeJson.ListEncodeJson" (encoderType t)
    Option t -> call "argonaut.EncodeJson.OptionEncodeJson" (encoderType t)
    Primitive p -> text (case p of
      PBoolean -> "argonaut.Json.jBool"
      PInt -> "argonaut.Json.jNumber"
      PDecimal -> "argonaut.EncodeJson.BigDecimalEncodeJson"
      PString -> "argonaut.Json.jString"
    )

encodeType :: Type -> Box -> Box
encodeType t e =
  encoderType t `paren1` [ e ]

recordFieldType :: RecordProp -> Box
recordFieldType (RecordProp n t) =
  text n <<>> char ':' <<+>> tyType t <<>> char ','

recordFieldEncoder :: RecordProp -> Box
recordFieldEncoder (RecordProp n t) =
  text ("\"" <> n <> "\" ->") <<+>> encodeType t (text ("x." <> n)) <<>> char ','
