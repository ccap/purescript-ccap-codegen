module Ccap.Codegen.PrettyPrint
  ( prettyPrint
  ) where

import Prelude

import Ccap.Codegen.Types (Module(..), Primitive(..), RecordProp(..), TypeOrRecord(..), Type(..), TypeDecl(..))
import Data.Array (length, mapWithIndex) as Array
import Text.PrettyPrint.Boxes (Box, char, emptyBox, left, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (top) as Boxes

prettyPrint :: Array Module -> String
prettyPrint modules =
  render $ vsep 1 Boxes.top (modules <#> oneModule)

oneModule :: Module -> Box
oneModule (Module name decls) =
  text ("module " <> name <> " {")
    // commaList decls typeDecl
    // text "}"

primitive :: Primitive -> Box
primitive p = text
  case p of
    PBoolean -> "boolean"
    PDate -> "date"
    PDateTime -> "dateTime"
    PInt -> "int"
    PDecimal -> "decimal"
    PString -> "string"
    PTime -> "time"

indented :: Box -> Box
indented b = emptyBox 0 2 <<>> b

indentedList :: Array Box -> Box
indentedList = indented <<< vcat Boxes.top

typeDecl :: Boolean -> TypeDecl -> Box
typeDecl last (TypeDecl name (Type t)) =
  text "type" <<+>> text name <<>> char ':' <<+>> tyType t <<>> commaExceptLast last
typeDecl last (TypeDecl name (Record props)) =
  text "type" <<+>> text name <<>> char ':' <<+>> char '{'
    // commaList props recordProp
    // (text "}" <<>> commaExceptLast last)

commaList
  :: forall a
   . Array a
  -> (Boolean -> a -> Box)
  -> Box
commaList as f =
  let l = Array.length as
  in indentedList $
    flip Array.mapWithIndex as \i a ->
      f (l == i + 1) a

commaExceptLast :: Boolean -> Box
commaExceptLast b =
  if b
    then emptyBox 0 0
    else char ','

recordProp :: Boolean -> RecordProp -> Box
recordProp last (RecordProp s t) =
  text s <<>> char ':' <<+>> tyType t <<>> commaExceptLast last

tyType :: Type -> Box
tyType = case _ of
  Primitive p -> primitive p
  Ref _ s -> text s
  Array t -> text "array" <<+>> tyType t
  Option t ->  text "optional" <<+>> tyType t
  Sum vs -> vcat left (vs <#> (\x -> text "| " <<+>> text x))
