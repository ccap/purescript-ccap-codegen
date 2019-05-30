module Ccap.Codegen.Purescript
  ( prettyPrint
  ) where

import Prelude

import Ccap.Codegen.Types (Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Data.Array ((:))
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hsep, left, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (bottom, top) as Boxes

data Emit box = Emit { imports :: Array String, out :: box }

instance functorEmit :: Functor Emit where
  map f (Emit { imports, out }) = Emit { imports, out: f out }

instance applyEmit :: Apply Emit where
  apply (Emit { imports: ib, out: f }) (Emit { imports: ia, out: a }) =
    Emit { imports: Array.union ia ib, out: f a }

instance applicativeEmit :: Applicative Emit where
  pure = emit mempty

instance bindEmit :: Bind Emit where
  bind (Emit { imports, out }) f =
    let Emit { imports: ib, out: b } = f out
    in Emit { imports: Array.union imports ib, out: b }

emit :: forall out. Array String -> out -> Emit out
emit imports out = Emit { imports, out }

prettyPrint :: String -> Array Module -> String
prettyPrint module_ modules =
  render $ vsep 1 Boxes.top (modules <#> oneModule module_)

oneModule :: String -> Module -> Box
oneModule module_ (Module name decls) = vsep 1 left do
  let es = decls <#> typeDecl true
  let is = es >>= \(Emit { imports }) -> imports
  let os = es >>= \(Emit { out }) -> [ out ]
  text ("module " <> module_ <> "." <> name <> " where")
    : vcat left (is <#> \i -> text ("import " <> i))
    : os

primitive :: Primitive -> Box
primitive p = text
  case p of
    PBoolean -> "Boolean"
    PInt -> "Int"
    PDecimal -> "Number" -- ish
    PString -> "String"

indent :: Box
indent = emptyBox 0 2

indented :: Box -> Box
indented = (<<>>) indent

type Extern = { prefix :: String, t :: String}

externalType :: Extern -> Emit Box
externalType { prefix, t } = Emit
  { imports: pure (prefix <> " (" <> t <> ")")
  , out: text t
  }

splitType :: String -> Maybe Extern
splitType s = do
  i <- String.lastIndexOf (Pattern ".") s
  let prefix = String.take i s
  let t = String.drop (i + 1) s
  pure $ { prefix, t }

typeDecl :: Boolean -> TypeDecl -> Emit Box
typeDecl last (TypeDecl name tt) =
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
        (hsep 1 Boxes.bottom $ vcat left <$> [ Array.drop 1 vs <#> \_ -> char '|',  vs <#> text ])

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

record :: Array RecordProp -> Emit Box
record props = do
  let len = Array.length props
  let space = emptyBox 0 1
  types <- (\(RecordProp _ t) -> tyType t) `traverse` props
  let columns =
        [ Array.snoc ('{' : (Array.drop 1 props <#> const ',')) '}' <#> char
        , props <#> \(RecordProp name _) -> text name
        , props <#> const (text "::")
        , types
        ] <#> vcat left
  pure (hsep 1 left columns)
