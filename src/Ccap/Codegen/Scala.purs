module Ccap.Codegen.Scala
  ( outputSpec
  , prettyPrint
  ) where

import Prelude

import Ccap.Codegen.Shared (OutputSpec, indented)
import Ccap.Codegen.Types (Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Data.Array (snoc, (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Text.PrettyPrint.Boxes (Box, char, left, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (top) as Boxes

prettyPrint :: String -> Array Module -> String
prettyPrint package modules =
  render $ vsep 1 Boxes.top (modules <#> oneModule package)

outputSpec :: String -> OutputSpec
outputSpec package =
  { render: render <<< oneModule package
  , fileName: \(Module n _) -> n <> ".scala"
  }

oneModule :: String -> Module -> Box
oneModule package (Module name decls) = vsep 1 left do
  text ("package " <> package)
    : text ("object " <> name <> " {")
    : (decls <#> typeDecl true >>> indented)
    `snoc` text "}"

primitive :: Primitive -> Box
primitive p = text
  case p of
    PBoolean -> "Boolean"
    PInt -> "Int"
    PDecimal -> "BigDecimal"
    PString -> "String"

typeDecl :: Boolean -> TypeDecl -> Box
typeDecl last (TypeDecl name tt) =
  case tt of
    Type t ->
      text "type" <<+>> text name <<+>> char '=' <<+>> tyType t
    Wrap t wo ->
      case Map.lookup "scala" wo of
        Nothing ->
          let tagname = text (name <> "T")
          in vcat left
            [ text "final abstract class" <<+>> tagname
            , text "type" <<+>> text name <<+>> char '='
              <<+>> text "scalaz.@@[" <<>> tyType t <<>> char ',' <<+>> tagname <<>> char ']'
            ]
        Just { typ, wrap, unwrap } ->
          text "type" <<+>> text name <<+>> char '=' <<+>> text typ
    Record props ->
      text "final case class" <<+>> text name <<>> char '('
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
