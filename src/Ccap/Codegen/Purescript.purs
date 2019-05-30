module Ccap.Codegen.Purescript
  ( outputSpec
  , prettyPrint
  ) where

import Prelude

import Ccap.Codegen.Shared (OutputSpec, indented)
import Ccap.Codegen.Types (Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Control.Monad.Writer (Writer, WriterT(..), runWriter)
import Data.Array ((:))
import Data.Array as Array
import Data.Identity (Identity(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hsep, left, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (bottom, top) as Boxes

type Imports = Array String

type Emit box = Writer Imports box

emit :: forall out. Array String -> out -> Emit out
emit imports out = WriterT (Identity (Tuple out imports))

prettyPrint :: String -> Array Module -> String
prettyPrint module_ modules =
  render $ vsep 1 Boxes.left (modules <#> oneModule module_)

oneModule :: String -> Module -> Box
oneModule module_ (Module name decls) = vsep 1 Boxes.left do
  let es = decls <#> typeDecl true <#> runWriter
      is = es >>= snd >>> Array.sort >>> Array.nub
      os = es >>= fst >>> pure
  text ("module " <> module_ <> "." <> name <> " where")
    : vcat left (is <#> \i -> text ("import " <> i))
    : os

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

typeDecl :: TypeDecl -> Box
typeDecl (TypeDecl name tt _) =
  let dec kw = text kw <<+>> text name <<+>> char '='
  in case tt of
    Type t ->
      tyType t <#> (dec "type" <<+>> _)
    Wrap t wo ->
      case Map.lookup "purs" wo of
        Nothing ->
          tyType t <#> ((dec "newtype" <<+>> text name) <<+>> _)
        Just { typ, wrap, unwrap } ->
          fromMaybe { prefix: "?missingPrefix", t: typ } (splitType typ)
            # externalType <#> (dec "type" <<+>> _)
    Record props ->
      record props <#> \p -> dec "type" // indented p
    Sum vs -> pure do
      dec "data" // indented
        (hsep 1 Boxes.bottom $ vcat Boxes.left <$> [ Array.drop 1 vs <#> \_ -> char '|',  vs <#> text ])

tyType :: Type -> Emit Box
tyType =
  let
    wrap tycon t =
      tyType t <#> \ty -> text tycon <<+>> char '(' <<>> ty <<>> char ')'
  in case _ of
    Primitive p -> pure $ primitive p
    Ref _ s -> fromMaybe (text s # pure) (splitType s <#> externalType)
    Array t -> wrap "Array" t
    Option t -> emit (pure "Data.Maybe (Maybe)") unit >>= const (wrap "Maybe" t)

record :: Array RecordProp -> Box
record props = do
  let len = length props
      space = emptyBox 0 1
      columns =
        [ snoc ('{' : (drop 1 props <#> const ',')) '}' <#> char
        , props <#> \(RecordProp name _) -> text name
        , props <#> const (text "::")
        , props <#> \(RecordProp _ t) -> tyType t
        ] <#> vcat Boxes.left
  pure (hsep 1 Boxes.left columns)
