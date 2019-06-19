module Ccap.Codegen.Scala
  ( outputSpec
  ) where

import Prelude

import Ccap.Codegen.Annotations (getWrapOpts)
import Ccap.Codegen.Shared (DelimitedLiteralDir(..), ExtraImports, OutputSpec, delimitedLiteral, indented)
import Ccap.Codegen.Types (Imports, Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (State, evalState, get)
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (traverse)
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hcat, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (left, top) as Boxes

type Env =
  { allModules :: Array Module
  , currentModule :: Module
  }

type Codegen = ReaderT Env (State ExtraImports)

runCodegen :: forall a. Module -> Array Module -> Codegen a -> a
runCodegen currentModule allModules c =
  evalState (runReaderT c { allModules, currentModule }) []

outputSpec :: String -> Array Module -> OutputSpec
outputSpec package modules =
  { render: render <<< oneModule package modules
  , fileName: \(Module n _ _) -> n <> ".scala"
  }

oneModule :: String -> Array Module -> Module -> Box
oneModule package all mod@(Module name imps decls) = runCodegen mod all do
  declsOutput <- traverse typeDecl decls
  importOutput <- imports package imps
  pure $
    vsep 1 Boxes.left do
      text ("package " <> package)
        : importOutput
        : standardImports
        : text ("object " <> name <> " {")
        : (declsOutput <#> indented)
        `Array.snoc` text "}"

curly :: Box -> Array Box -> Box
curly pref inner =
  vcat Boxes.left (pref <<+>> char '{' : (indented <$> inner) `Array.snoc` char '}')

paren :: Box -> Array Box -> Box
paren pref inner =
  vcat Boxes.left (pref <<>> char '(' : (indented <$> inner) `Array.snoc` char ')')

paren_ :: Box -> Array Box -> Box -> Box
paren_ pref inner suffix =
  vcat Boxes.left (pref <<>> char '(' : (indented <$> inner) `Array.snoc` (char ')' <<>> suffix))

-- TODO: Clean up when we switch to a proper pretty printer.
-- Like `paren`, but outputs on a sigle line.
paren1 :: Box -> Array Box -> Box
paren1 pref inner =
  hcat Boxes.top (pref <<>> char '(' : inner `Array.snoc` char ')')

standardImports :: Box
standardImports =
  text "import gov.wicourts.jsoncommon.Encoder"
    // text "import gov.wicourts.jsoncommon.Decoder"
    // text "import scalaz.Monad"

imports :: String -> Imports -> Codegen Box
imports package imps = do
  extra <- get
  -- TODO: Check for alternate package name in annotation. (if we need to import
  --       modules from other packages)
  -- let all = ((imps <#> \(Import s) -> package <> "." <> s) <> extra) # Array.sort >>> Array.nub
  let all = extra # Array.sort >>> Array.nub
  pure $ vcat Boxes.left (all <#> \s -> text ("import " <> s))

defEncoder :: String -> Box -> Box
defEncoder name enc =
  text ("lazy val jsonEncoder" <> name <> ": Encoder[" <> name <> ", argonaut.Json] =")
    // indented enc

defDecoder :: String -> String -> Box -> Box
defDecoder name dType dec =
  text ("def jsonDecoder" <> name <> "[M[_]: Monad]: Decoder." <> dType <> "[M, " <> name <> "] =")
    // indented dec

wrapEncoder :: String -> Type -> Box -> Box
wrapEncoder name t enc = defEncoder name do
  (encoder t <<>> text ".compose") `paren` [ enc ]

wrapDecoder :: String -> Type -> Box -> Codegen Box
wrapDecoder name t dec = do
  d <- decoderType t
  let body = (decoder t <<>> text ".disjunction.andThen") `paren` [ dec ] // text ".validation"
  pure $ defDecoder name d body

typeDecl :: TypeDecl -> Codegen Box
typeDecl (TypeDecl name tt an) =
  case tt of
    Type t -> do
      d <- decoderType t
      pure $
        text "type" <<+>> text name <<+>> char '=' <<+>> tyType t
          // defEncoder name (encoder t)
          // defDecoder name d (decoder t)
    Wrap t -> do
      d <- decoderType t
      case getWrapOpts "scala" an of
        Nothing ->
          let
            tagname = text (name <> "T")
            scalatyp = text"scalaz.@@[" <<>> tyType t <<>> char ',' <<+>> tagname <<>> char ']'
          in pure $ vcat Boxes.left
            [ text "final abstract class" <<+>> tagname
            , text "type" <<+>> text name <<+>> char '=' <<+>> scalatyp
            , defEncoder name (encoder t <<>> text ".tagged")
            , defDecoder name d (decoder t <<>> text ".tagged")
            ]
        Just { typ, decode, encode } -> do
          wrappedDecoder <- wrapDecoder name t (text decode <<>> text ".disjunction")
          pure $
            text "type" <<+>> text name <<+>> char '=' <<+>> text typ
              // wrapEncoder name t (text encode)
              // wrappedDecoder
    Record props ->
      let
        cls = (text "final case class" <<+>> text name) `paren` (recordFieldType <$> props)
        enc = defEncoder name (text "x => argonaut.Json.obj" `paren` (recordFieldEncoder <$> props))
        decBody =
          case Array.length props of
            1 -> maybe (emptyBox 0 0) (singletonRecordDecoder name) (Array.head props)
            x | x <= 12 -> smallRecordDecoder name props
            x -> bigRecordDecoder name props
        dec = defDecoder name "Form" decBody
      in pure $ cls // enc // dec
    Sum vs -> do
      let
        trait = (text "sealed trait" <<+>> text name) `curly` [ text "def tag: String"]
        variants = vs <#> \v ->
          text ("case object " <> v <> " extends " <> name)
            `curly` [ text ("override def tag: String = " <> show v)]
        assocs = vs <#> \v ->
          paren1 (emptyBox 0 0) [ text (show v), text ", ", text name <<>> char '.' <<>> text v ] <<>> char ','
        params = text (show name) <<>> char ',' : assocs
        enc = wrapEncoder name (Primitive PString) (text "_.tag")
      dec <- wrapDecoder
              name
              (Primitive PString)
              (((text ("Decoder.enum[M, " <> name) <<>> char ']') `paren` params) // text ".disjunction")
      pure $ trait // ((text "object" <<+>> text name) `curly` variants) // enc // dec

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

encoderDecoder :: String -> Type -> Box
encoderDecoder which ty =
  case ty of
    Ref _ { mod, typ } -> text (maybe "" (_ <> ".") mod <> "json" <> which <> typ)
    Array t -> encoderDecoder which t <<>> text ".list"
    Option t -> encoderDecoder which t <<>> text ".option"
    Primitive p -> text (case p of
      PBoolean -> which <> ".boolean"
      PInt -> which <> ".int"
      PDecimal -> which <> ".decimal"
      PString -> which <> ".string"
    )

encoder :: Type -> Box
encoder = encoderDecoder "Encoder"

decoder :: Type -> Box
decoder = encoderDecoder "Decoder"

decoderType :: Type -> Codegen String
decoderType ty =
  case ty of
    Ref _ { mod, typ } -> do
      { currentModule, allModules } <- ask
      let external = mod >>= (\m -> Array.find (\(Module n _ _) -> n == m) allModules)
          Module _ _ ds = fromMaybe currentModule external
          tt = Array.find (\(TypeDecl n _ _) -> n == typ) ds
                <#> (\(TypeDecl _ t _) -> t)
      maybe (pure "MISSING") decoderTopType tt
    Array t -> decoderType t
    Option t -> decoderType t
    Primitive _ -> pure "Field"

decoderTopType :: TopType -> Codegen String
decoderTopType = case _ of
  Type ty -> decoderType ty
  Wrap ty -> decoderType ty
  Record _ -> pure "Form"
  Sum _ -> pure "Field"

encodeType :: Type -> Box -> Box
encodeType t e =
  encoder t <<>> text ".encode" `paren1` [ e ]

recordFieldType :: RecordProp -> Box
recordFieldType (RecordProp n t) =
  text n <<>> char ':' <<+>> tyType t <<>> char ','

recordFieldEncoder :: RecordProp -> Box
recordFieldEncoder (RecordProp n t) =
  text (show n <> " ->") <<+>> encodeType t (text ("x." <> n)) <<>> char ','

recordFieldDecoder :: RecordProp -> Box
recordFieldDecoder (RecordProp n t) =
  decoder t <<>> text ".property(" <<>> text (show n) <<>> char ')'

singletonRecordDecoder :: String -> RecordProp -> Box
singletonRecordDecoder name prop =
  recordFieldDecoder prop <<>> text (".map(" <> name <> ".apply)")

smallRecordDecoder :: String -> Array RecordProp -> Box
smallRecordDecoder name props = paren_
  (text ("scalaz.Apply[Decoder.Form[M, ?]].apply" <> show (Array.length props)))
  (props <#> \r -> recordFieldDecoder r <<>> char ',')
  (text ("(" <> name <> ".apply)"))

bigRecordDecoder :: String -> Array RecordProp -> Box
bigRecordDecoder name props =
  paren_
    (text ("scalaz.Apply[Decoder.Form[M, ?]].apply" <> show (Array.length parts)))
    (parts <#> \part ->
      if Array.length part == 1
        then
          maybe (emptyBox 0 0) (\r -> recordFieldDecoder r <<>> char ',') (Array.head part)
        else
          (paren_
            (text ("scalaz.Apply[Decoder.Form[M, ?]].tuple" <> show (Array.length part)))
            (part <#> \r -> recordFieldDecoder r <<>> char ','))
            (char ','))
    (curly (emptyBox 0 0) [ applyAllParams ])
  where
    parts = chunksOf 5 props
    applyAllParams =
      paren_
        (text "case ")
        (parts <#> \part ->
            -- No trailing commas allowed when matching a tuple pattern
            (delimitedLiteral Horiz '(' ')' (part <#> \(RecordProp n _) -> text n)) <<>> char ',')
        (text " =>" // indented applyAllConstructor)
    applyAllConstructor =
      paren (text name) (props <#> \(RecordProp n _) -> text (n <> " = " <> n <> ","))

chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n as =
  Array.range 0 ((Array.length as - 1) / n) <#> \i ->
    Array.slice (i*n) (i*n + n) as
