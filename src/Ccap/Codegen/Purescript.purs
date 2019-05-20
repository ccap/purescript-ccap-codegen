module Ccap.Codegen.Purescript
  ( prettyPrint
  ) where

import Prelude

import Ccap.Codegen.Types (Module(..), Primitive(..), RecordProp(..), TypeOrRecord(..), Type(..), TypeDecl(..))
import Data.Array (drop, length, snoc, (:))
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hsep, left, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (bottom, top) as Boxes

prettyPrint :: Array Module -> String
prettyPrint modules =
  render $ vsep 1 Boxes.top (modules <#> oneModule)

oneModule :: Module -> Box
oneModule (Module name decls) = vsep 1 left do
  text ("module " <> name <> " where")
    : text "import Data.Maybe (Maybe)"
    : (decls <#> typeDecl true)

primitive :: Primitive -> Box
primitive p = text
  case p of
    PBoolean -> "Boolean"
    PInt -> "Int"
    PDecimal -> "Number" -- ish
    PString -> "String"
    -- TODO
    PDate -> "date"
    PDateTime -> "dateTime"
    PTime -> "time"

indent :: Box
indent = emptyBox 0 2

indented :: Box -> Box
indented = (<<>>) indent

typeDecl :: Boolean -> TypeDecl -> Box
typeDecl last (TypeDecl name tt) =
  let dec kw = text kw <<+>> text name <<+>> char '='
  in case tt of
    Type t ->
      dec "type" <<+>> tyType t
    Record props ->
      dec "type" // indented (record props)
    Sum vs ->
      dec "data" // indented (
        hsep 1 Boxes.bottom $ vcat left <$> [ drop 1 vs <#> \_ -> char '|',  vs <#> text ]
        )


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
      ] <#> vcat left
  in hsep 1 left columns
