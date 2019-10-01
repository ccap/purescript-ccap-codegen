module Ccap.Codegen.Scala
  ( outputSpec
  ) where

import Prelude

import Ccap.Codegen.Annotations (getMaxLength, getWrapOpts, field) as Annotations
import Ccap.Codegen.Shared (DelimitedLiteralDir(..), OutputSpec, Env, delimitedLiteral, indented)
import Ccap.Codegen.Types (Annotations, Module, Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..), ValidatedModule, isRecord)
import Control.Monad.Reader (Reader, ask, asks, runReader)
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Traversable (for, traverse)
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hcat, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (left, top) as Boxes

type Codegen = Reader Env

runCodegen :: forall a. Env -> Codegen a -> a
runCodegen = flip runReader

outputSpec :: String -> Array Module -> OutputSpec
outputSpec defaultPackage modules = --remove defaultPackage
  { render: render <<< oneModule modules
  , filePath: \mod ->
      let path = String.replaceAll
                    (String.Pattern ".")
                    (String.Replacement "/")
                    (mod.exports.scalaPkg)
      in path <> "/" <> mod.name <> ".scala"
  }

oneModule :: Array Module -> ValidatedModule -> Box
oneModule all mod = do
  let modDecl = Array.find (\(TypeDecl n tt _) -> n == mod.name && isRecord tt) mod.types
      env = { defaultPrefix: "", currentModule: mod { imports = mod.imports <#> _.scalaPkg }, allModules: all }
      body = runCodegen env do
        modDeclOutput <- traverse (typeDecl TopLevelCaseClass) modDecl
        declsOutput <- traverse (typeDecl CompanionObject) mod.types
        pure $
          Array.fromFoldable modDeclOutput
            <> [ text ("object " <> mod.name <> " {") ]
            <> (declsOutput <#> indented)
            <> [ char '}']
  vsep 1 Boxes.left do
    [ text "// This file is automatically generated. Do not edit."
    , text ("package " <> mod.exports.scalaPkg)
    , imports (mod.imports <#> _.scalaPkg)
    ] <> body

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

standardImports :: Array String
standardImports =
  [ "gov.wicourts.jsoncommon.Encoder"
  , "gov.wicourts.jsoncommon.Decoder"
  , "scalaz.Monad"
  ]

imports :: Array String -> Box
imports impts =
  let all = impts <> standardImports # Array.sort >>> Array.nub
  in vcat Boxes.left (all <#> \s -> text ("import " <> s))

defEncoder :: Boolean -> String -> Box -> Box
defEncoder includeName name enc =
  let includedName = if includeName then name else ""
  in
    text ("lazy val jsonEncoder" <> includedName <> ": Encoder[" <> name <> ", argonaut.Json] =")
      // indented enc

defDecoder :: Boolean -> String -> String -> Box -> Box
defDecoder includeName name dType dec =
  let includedName = if includeName then name else ""
  in
    text ("def jsonDecoder" <> includedName <> "[M[_]: Monad]: Decoder." <> dType <> "[M, " <> name <> "] =")
      // indented dec

wrapEncoder :: String -> Type -> Box -> Codegen Box
wrapEncoder name t enc = do
  e <- encoder t
  pure $ defEncoder true name ((e <<>> text ".compose") `paren` [ enc ])

wrapDecoder :: Annotations -> String -> Type -> Box -> Codegen Box
wrapDecoder annots name t dec = do
  d <- decoderType t
  topDec <- topDecoder annots t
  let body = (topDec <<>> text ".disjunction.andThen") `paren` [ dec ] // text ".validation"
  pure $ defDecoder true name d body

data TypeDeclOutputMode = TopLevelCaseClass | CompanionObject
derive instance eqTypeDeclOutputMode :: Eq TypeDeclOutputMode

typeDecl :: TypeDeclOutputMode -> TypeDecl -> Codegen Box
typeDecl outputMode (TypeDecl name tt an) =
  case tt of
    Type t -> do
      dTy <- decoderType t
      ty <- tyType Nothing t
      e <- encoder t
      d <- topDecoder an t
      pure $
        text "type" <<+>> text name <<+>> char '=' <<+>> ty
          // defEncoder true name e
          // defDecoder true name dTy d
    Wrap t -> do
      case Annotations.getWrapOpts "scala" an of
        Nothing -> do
          dTy <- decoderType t
          ty <- tyType Nothing t
          e <- encoder t
          d <- topDecoder an t
          let
            tagname = text (name <> "T")
            scalatyp = text"scalaz.@@[" <<>> ty <<>> char ',' <<+>> tagname <<>> char ']'
          pure $ vcat Boxes.left
            [ text "final abstract class" <<+>> tagname
            , text "type" <<+>> text name <<+>> char '=' <<+>> scalatyp
            , text "val" <<+>> text name <<>> char ':' <<+>> text "scalaz.Tag.TagOf["
                <<>> tagname <<>> text "] = scalaz.Tag.of[" <<>> tagname <<>> char ']'
            , defEncoder true name (e <<>> text ".tagged")
            , defDecoder true name dTy (d <<>> text ".tagged")
            ]
        Just { typ, decode, encode } -> do
          wrappedEncoder <-  wrapEncoder name t (text encode)
          wrappedDecoder <- wrapDecoder an name t (text decode <<>> text ".disjunction")
          pure $
            text "type" <<+>> text name <<+>> char '=' <<+>> text typ
              // wrappedEncoder
              // wrappedDecoder
    Record props -> do
      mod <- asks _.currentModule
      let
        qualify =
          if mod.name == name && outputMode == TopLevelCaseClass
            then Just mod.name
            else Nothing
      recordFieldTypes <- traverse (recordFieldType qualify) props
      recordFieldEncoders <- traverse recordFieldEncoder props
      let
        cls = (text "final case class" <<+>> text name) `paren` recordFieldTypes
        enc = defEncoder (mod.name /= name) name (text "x => argonaut.Json.obj" `paren` recordFieldEncoders)
      decBody <-
        case Array.length props of
          1 -> maybe (pure (emptyBox 0 0)) (singletonRecordDecoder name) (Array.head props)
          x | x <= 12 -> smallRecordDecoder name props
          x -> bigRecordDecoder name props
      let
        dec = defDecoder (mod.name /= name) name "Form" decBody
        fieldNamesTarget =
          if mod.name == name
            then Nothing
            else Just name
        names = fieldNames fieldNamesTarget (props <#> \(RecordProp n _) -> n)
        output | mod.name == name && outputMode == TopLevelCaseClass = cls
        output | mod.name == name && outputMode == CompanionObject = enc // dec // names
        output | otherwise = cls // enc // dec // names
      pure output
    Sum vs -> do
      let
        trait = (text "sealed trait" <<+>> text name) `curly` [ text "def tag: String"]
        variants = vs <#> \v ->
          text ("case object " <> v <> " extends " <> name)
            `curly` [ text ("override def tag: String = " <> show v)]
        assocs = vs <#> \v ->
          paren1 (emptyBox 0 0) [ text (show v), text ", ", text name <<>> char '.' <<>> text v ] <<>> char ','
        params = text (show name) <<>> char ',' : assocs
      enc <- wrapEncoder name (Primitive PString) (text "_.tag")
      dec <- wrapDecoder
              an
              name
              (Primitive PString)
              (((text ("Decoder.enum[M, " <> name) <<>> char ']') `paren` params) // text ".disjunction")
      pure $ trait // ((text "object" <<+>> text name) `curly` variants) // enc // dec

fieldNames :: Maybe String -> Array String -> Box
fieldNames mod names =
  maybe body (\m -> curly (text "object" <<+>> text m) [ body ]) mod
  where
    body = curly (text "object" <<+>> text "FieldNames") (names <#> fieldNameConst)
    fieldNameConst s =
      text "val" <<+>> text (valName s) <<>> text ": String" <<+>> text "=" <<+>> text (show s)
    valName s =
      let { before, after } = String.splitAt 1 s
      in String.toUpper before <> after

tyType :: Maybe String -> Type -> Codegen Box
tyType qualify ty = do
  allModules <- asks _.allModules
  let wrap tycon t = do
        ty_ <- tyType qualify t
        pure $ text tycon <<>> char '[' <<>> ty_ <<>> char ']'
  case ty of
    Ref _ { mod, typ } ->
      pure $ text (mod >>= fullyQualifiedRef allModules typ # fromMaybe (partiallyQualifiedRef qualify typ))
    Array t -> wrap "List" t
    Option t ->  wrap "Option" t
    Primitive p -> pure $ text (case p of
      PBoolean -> "Boolean"
      PInt -> "Int"
      PDecimal -> "BigDecimal"
      PString -> "String"
    )

partiallyQualifiedRef :: Maybe String -> String -> String
partiallyQualifiedRef q typ =
  maybe "" (_ <> ".") q <> typ

fullyQualifiedRef :: Array Module -> String -> String -> Maybe String
fullyQualifiedRef all typ modName = do
  { rec, pkg } <- refData all typ modName
  pure
    if modName == typ && rec
      then pkg <> typ
      else pkg <> modName <> "." <> typ

refData :: Array Module -> String -> String -> Maybe { rec :: Boolean, pkg :: String }
refData all typ modName = do
  mod <- Array.find (\mod -> mod.name == modName) all
  let rec = Array.find (\(TypeDecl n _ _) -> n == typ) mod.types
              <#> (\(TypeDecl _ tt _) -> isRecord tt)
              # fromMaybe false
      pkg = Annotations.field "scala" "package" mod.annots <#> (_ <> ".") # fromMaybe ""
  pure { rec, pkg }

encoderDecoderRef :: Array Module -> String -> String -> String -> Maybe String
encoderDecoderRef all typ which modName = do
  { rec, pkg } <- refData all typ modName
  let prefix = pkg <> modName <> ".json" <> which
  pure
    if modName == typ && rec
      then prefix
      else prefix <> typ

encoderDecoder :: String -> Type -> Codegen Box
encoderDecoder which ty = do
  allModules <- asks _.allModules
  case ty of
    Ref _ { mod, typ } -> do
      let jRef = "json" <> which <> typ
      pure $ text (mod >>= encoderDecoderRef allModules typ which # fromMaybe jRef)
    Array t -> encoderDecoder which t <#> (_ <<>> text ".list")
    Option t -> encoderDecoder which t <#> (_ <<>> text ".option")
    Primitive p -> pure $
      text (case p of
        PBoolean -> which <> ".boolean"
        PInt -> which <> ".int"
        PDecimal -> which <> ".decimal"
        PString -> which <> ".string"
      )

encoder :: Type -> Codegen Box
encoder = encoderDecoder "Encoder"

decoder :: Type -> Codegen Box
decoder = encoderDecoder "Decoder"

topDecoder :: Annotations -> Type -> Codegen Box
topDecoder annots ty = do
  let maxLength = Annotations.getMaxLength annots
  decoder ty <#> (_ <<>> (maybe (emptyBox 0 0) (\s -> text (".maxLength(" <> s <> ")")) maxLength))

decoderType :: Type -> Codegen String
decoderType ty =
  case ty of
    Ref _ { mod, typ } -> do
      { currentModule, allModules } <- ask
      let external = mod >>= (\m -> Array.find (\n -> n.name == m) allModules)
          myMod = fromMaybe currentModule external
          tt = Array.find (\(TypeDecl n _ _) -> n == typ) myMod.types
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

encodeType :: Type -> Box -> Codegen Box
encodeType t e =
  encoder t <#> (_ <<>> text ".encode" `paren1` [ e ])

recordFieldType :: Maybe String -> RecordProp -> Codegen Box
recordFieldType qualify (RecordProp n t) = do
  ty <- tyType qualify t
  pure $ text n <<>> char ':' <<+>> ty <<>> char ','

recordFieldEncoder :: RecordProp -> Codegen Box
recordFieldEncoder (RecordProp n t) = do
  ty <- encodeType t (text ("x." <> n))
  pure $ text (show n <> " ->") <<+>> ty <<>> char ','

recordFieldDecoder :: RecordProp -> Codegen Box
recordFieldDecoder (RecordProp n t) =
  decoder t <#> (_ <<>> text ".property(" <<>> text (show n) <<>> char ')')

singletonRecordDecoder :: String -> RecordProp -> Codegen Box
singletonRecordDecoder name prop =
  recordFieldDecoder prop <#> (_ <<>> text (".map(" <> name <> ".apply)"))

smallRecordDecoder :: String -> Array RecordProp -> Codegen Box
smallRecordDecoder name props = do
  ps <- traverse (\r -> recordFieldDecoder r <#> (_ <<>> char ',')) props
  pure $
    paren_
      (text ("scalaz.Apply[Decoder.Form[M, ?]].apply" <> show (Array.length props)))
      ps
      (text ("(" <> name <> ".apply)"))

bigRecordDecoder :: String -> Array RecordProp -> Codegen Box
bigRecordDecoder name props = do
  body <-
    for parts \part ->
      if Array.length part == 1
        then
          maybe
            (pure (emptyBox 0 0))
            (\r -> recordFieldDecoder r <#> (_ <<>> char ','))
            (Array.head part)
        else do
          decs <- traverse (\r -> recordFieldDecoder r <#> (_ <<>> char ',')) part
          pure $
            paren_
              (text ("scalaz.Apply[Decoder.Form[M, ?]].tuple" <> show (Array.length part)))
              decs
              (char ',')
  pure $
    paren_
      (text ("scalaz.Apply[Decoder.Form[M, ?]].apply" <> show (Array.length parts)))
      body
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
