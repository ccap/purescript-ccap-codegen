module Ccap.Codegen.Scala
  ( prettyPrint
  ) where

import Prelude

import Ccap.Codegen.Types (Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Data.Array (snoc, (:))
import Text.PrettyPrint.Boxes (Box, char, emptyBox, left, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (top) as Boxes

prettyPrint :: Array Module -> String
prettyPrint modules =
  render $ vsep 1 Boxes.top (modules <#> oneModule)

oneModule :: Module -> Box
oneModule (Module name decls) = vsep 1 left do
  text "package gov.wicourts.codegen" -- TODO: Make configurable
    : imports
    : text ("object " <> name <> " {")
    : (decls <#> typeDecl true)
    `snoc` text "}"

imports :: Box
imports =
  [ "org.joda.time.LocalDate"
  , "org.joda.time.LocalDateTime"
  , "org.joda.time.LocalTime"
  ] <#> (\i -> text ("import "<>i)) # vcat left

primitive :: Primitive -> Box
primitive p = text
  case p of
    PBoolean -> "Boolean"
    PInt -> "Int"
    PDecimal -> "BigDecimal"
    PString -> "String"
    PDate -> "LocalDate"
    PDateTime -> "LocalDateTime"
    PTime -> "LocalTime"

indent :: Box
indent = emptyBox 0 2

indented :: Box -> Box
indented = (<<>>) indent

typeDecl :: Boolean -> TypeDecl -> Box
typeDecl last (TypeDecl name tt) =
  case tt of
    Type t ->
      text "type" <<+>> text name <<+>> char '=' <<+>> tyType t
    Wrap t ->
      let tagname = text (name<>"T")
      in vcat left
        [ text "final abstract class" <<+>> tagname
        , text "type" <<+>> text name <<+>> char '='
          <<+>> text "scalaz.@@[" <<>> tyType t <<>> char ',' <<+>> tagname <<>> char ']'
        ]
    Record props ->
       text "case class" <<+>> text name <<>> char '('
       // indented (recordFields props)
       // char ')'
    Sum vs ->
      let
        variants = vcat left do
          v <- vs
          pure $ text ("case object " <> v <> " extends " <> name)
      in
      text "sealed trait" <<+>> text name
      // (text "object" <<+>> text name <<+>> char '{')
      // indented variants
      // char '}'

tyType :: Type -> Box
tyType =
  let wrap tycon t = text tycon <<>> char '[' <<>> tyType t <<>> char ']'
  in case _ of
    Primitive p -> primitive p
    Ref _ s -> text s
    Array t -> wrap "Array" t
    Option t ->  wrap "Option" t

recordFields :: Array RecordProp -> Box
recordFields props = vcat left (props <#> field)

field :: RecordProp -> Box
field (RecordProp n t) = text n <<>> char ':' <<+>> tyType t <<>> char ','
