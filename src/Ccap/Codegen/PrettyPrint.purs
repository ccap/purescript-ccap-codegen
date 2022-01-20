module Ccap.Codegen.PrettyPrint
  ( prettyPrint
  ) where

import Prelude
import Ccap.Codegen.Cst as Cst
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), maybe)
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hsep, render, text, vcat, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (left, top) as Boxes

prettyPrint :: Cst.Module -> String
prettyPrint = render <<< oneModule

oneModule :: Cst.Module -> Box
oneModule mod =
  text ("scala: " <> mod.exports.scalaPkg)
    // text ("purs: " <> mod.exports.pursPkg)
    // (vcat Boxes.left $ mod.imports <#> \(Cst.Import _ t) -> text ("import " <> t))
    // (vcat Boxes.left $ mod.types <#> typeDecl)

leadingSpace :: Array Box -> Box
leadingSpace boxes =
  ( if Array.length boxes > 0 then
      char ' '
    else
      emptyBox 0 0
  )
    <<>> hsep 1 Boxes.top boxes

primitive :: Cst.Primitive -> Box
primitive p =
  text case p of
    Cst.PBoolean -> "Boolean"
    Cst.PInt -> "Int"
    Cst.PDecimal -> "Decimal"
    Cst.PString -> "String"
    Cst.PStringValidationHack -> "StringValidationHack"
    Cst.PJson -> "Json"

indented :: Box -> Box
indented b = emptyBox 0 2 <<>> b

indentedList :: forall f. Foldable f => f Box -> Box
indentedList = indented <<< vcat Boxes.left

typeDecl :: Cst.TypeDecl -> Box
typeDecl (Cst.TypeDecl { name, topType: tt, annots, params }) =
  let
    dec :: Box
    dec = text "type" <<+>> text name <<>> ps params <<>> char ':'

    ps :: Array Cst.TypeParam -> Box
    ps = leadingSpace <<< map (\(Cst.TypeParam s) -> text s)

    ty = case tt of
      Cst.Type t -> dec <<+>> tyType t
      Cst.Wrap t -> dec <<+>> text "wrap" <<+>> tyType t
      Cst.Record props ->
        dec <<+>> char '{'
          // indentedList (props <#> recordProp)
          // text "}"
      Cst.Sum vs ->
        dec <<+>> char '['
          // indented (vcat Boxes.left (vs <#> (\x -> char '|' <<+>> constructor x)))
          // char ']'
  in
    ty // indentedList (annots <#> annotation)

constructor :: Cst.Constructor -> Box
constructor = case _ of
  Cst.NoArg (Cst.ConstructorName c) -> text c
  Cst.WithArgs (Cst.ConstructorName c) params -> text c <<+>> hsep 1 Boxes.top (map typeOrParam params)

annotation :: Cst.Annotation -> Box
annotation (Cst.Annotation name _ params) =
  let
    op = if Array.length params == 0 then (<<>>) else (<<+>>)
  in
    char '<' <<>> text name `op` (hsep 1 Boxes.top (params <#> annotationParam)) <<>> char '>'

annotationParam :: Cst.AnnotationParam -> Box
annotationParam (Cst.AnnotationParam name _ value) = text name <<>> maybe (emptyBox 0 0) ((char '=' <<>> _) <<< text <<< show) value

recordProp :: Cst.RecordProp -> Box
recordProp { name, typ, annots } =
  text name <<>> char ':' <<+>> typeOrParam typ <<>> (if Array.null annots then char ',' else emptyBox 0 0)
    // indentedList (annotation <$> annots)

typeOrParam :: Cst.TypeOrParam -> Box
typeOrParam = case _ of
  Cst.TType t -> tyType t
  Cst.TParam (Cst.TypeParam t) -> text t

tyType :: Cst.Type -> Box
tyType = case _ of
  Cst.Primitive p -> primitive p
  Cst.Ref _ { mod: Nothing, typ, params: [] } -> text typ
  Cst.Ref _ { mod: Just (Cst.ModuleRef m), typ, params: [] } -> text (m <> "." <> typ)
  Cst.Ref _ { mod: Nothing, typ, params } -> text typ <<+>> hsep 1 Boxes.top (map typeOrParam params)
  Cst.Ref _ { mod: Just (Cst.ModuleRef m), typ, params } -> text (m <> "." <> typ) <<+>> hsep 1 Boxes.top (map typeOrParam params)
  Cst.Array t -> text "Array" <<+>> typeOrParam t
  Cst.Option t -> text "Maybe" <<+>> typeOrParam t
  Cst.TypeWithParens t -> char '(' <<>> tyType t <<>> char ')'
