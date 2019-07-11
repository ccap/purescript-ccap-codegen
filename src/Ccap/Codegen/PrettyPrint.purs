module Ccap.Codegen.PrettyPrint
  ( outputSpec
  , prettyPrint
  ) where

import Prelude

import Ccap.Codegen.Shared (OutputSpec)
import Ccap.Codegen.Types (Annotation(..), AnnotationParam(..), Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hsep, render, text, vcat, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (left, top) as Boxes

prettyPrint :: Module -> String
prettyPrint = render <<< oneModule

outputSpec :: OutputSpec
outputSpec =
  { render: render <<< oneModule
  , filePath: \(Module n _ _) -> n <> ".tmpl"
  }

oneModule :: Module -> Box
oneModule (Module name decls annots) =
  text ("module " <> name) <<+>> trailingSpace (annots <#> annotation) <<>> text "{"
    // indentedList (decls <#> typeDecl)
    // text "}"

trailingSpace :: Array Box -> Box
trailingSpace boxes =
  hsep 1 Boxes.top boxes <<>>
    if Array.length boxes > 0 then char ' ' else emptyBox 0 0

primitive :: Primitive -> Box
primitive p = text
  case p of
    PBoolean -> "Boolean"
    PInt -> "Int"
    PDecimal -> "Decimal"
    PString -> "String"

indented :: Box -> Box
indented b = emptyBox 0 2 <<>> b

indentedList :: Array Box -> Box
indentedList = indented <<< vcat Boxes.left

typeDecl :: TypeDecl -> Box
typeDecl (TypeDecl name tt annots) =
  let
    dec = text "type" <<+>> text name <<>> char ':'
    ty = case tt of
      Type t ->
        dec <<+>> tyType t
      Wrap t ->
        dec <<+>> text "wrap" <<+>> tyType t
      Record props ->
        dec <<+>> char '{'
          // indentedList (props <#> recordProp)
          // text "}"
      Sum vs ->
        dec <<+>> char '['
          // indented (vcat Boxes.left (vs <#> (\x -> char '|' <<+>> text x)))
          // char ']'
  in ty // indentedList (annots <#> annotation)

annotation :: Annotation -> Box
annotation (Annotation name _ params) =
  let op = if Array.length params == 0 then (<<>>) else (<<+>>)
  in char '<' <<>> text name `op` (hsep 1 Boxes.top (params <#> annotationParam)) <<>> char '>'

annotationParam :: AnnotationParam -> Box
annotationParam (AnnotationParam name _ value) =
 text name <<>> maybe (emptyBox 0 0) ((char '=' <<>> _) <<< text <<< show) value

recordProp :: RecordProp -> Box
recordProp (RecordProp s t) =
  text s <<>> char ':' <<+>> tyType t

tyType :: Type -> Box
tyType = case _ of
  Primitive p -> primitive p
  Ref _ { mod: Nothing, typ } -> text typ
  Ref _ { mod: Just m, typ } -> text (m <> "." <> typ)
  Array t -> text "Array" <<+>> tyType t
  Option t ->  text "Maybe" <<+>> tyType t
