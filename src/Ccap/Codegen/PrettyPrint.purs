module Ccap.Codegen.PrettyPrint
  ( prettyPrint
  ) where

import Prelude

import Ccap.Codegen.Types (Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Text.PrettyPrint.Boxes (Box, char, emptyBox, left, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (top) as Boxes

prettyPrint :: Array Module -> String
prettyPrint modules =
  render $ vsep 1 Boxes.top (modules <#> oneModule)

oneModule :: Module -> Box
oneModule (Module name decls) =
  text ("module " <> name <> " {")
    // indentedList (decls <#> typeDecl)
    // text "}"

primitive :: Primitive -> Box
primitive p = text
  case p of
    PBoolean -> "boolean"
    PInt -> "int"
    PDecimal -> "decimal"
    PString -> "string"

indented :: Box -> Box
indented b = emptyBox 0 2 <<>> b

indentedList :: Array Box -> Box
indentedList = indented <<< vcat Boxes.top

typeDecl :: TypeDecl -> Box
typeDecl (TypeDecl name tt) =
  let dec = text "type" <<+>> text name <<>> char ':'
  in case tt of
    Type t ->
      dec <<+>> tyType t
    Wrap t wo ->
      dec <<+>> text "wrap" <<+>> tyType t
    Record props ->
      dec <<+>> char '{'
        // indentedList (props <#> recordProp)
        // text "}"
    Sum vs ->
      dec <<+>> char '['
        // indented (vcat left (vs <#> (\x -> text "| " <<+>> text x)))
        // char ']'

recordProp :: RecordProp -> Box
recordProp (RecordProp s t) =
  text s <<>> char ':' <<+>> tyType t

tyType :: Type -> Box
tyType = case _ of
  Primitive p -> primitive p
  Ref _ s -> text s
  Array t -> text "array" <<+>> tyType t
  Option t ->  text "optional" <<+>> tyType t
