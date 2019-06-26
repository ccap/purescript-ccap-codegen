module Ccap.Codegen.Purescript
  ( outputSpec
  ) where

import Prelude

import Ccap.Codegen.Annotations (field) as Annotations
import Ccap.Codegen.Annotations (getWrapOpts)
import Ccap.Codegen.Shared (DelimitedLiteralDir(..), OutputSpec, Env, delimitedLiteral, indented)
import Ccap.Codegen.Types (Annotations, Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Control.Apply (lift2)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer (class MonadTell, Writer, runWriter, tell)
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Filterable (compact)
import Data.Foldable (any, intercalate)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (for, for_, traverse)
import Data.Tuple (Tuple(..))
import Text.PrettyPrint.Boxes (Box, char, hsep, render, text, vcat, vsep, (//), (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (bottom, left) as Boxes

type PsImport =
  { mod :: String
  , typ :: Maybe String
  , alias :: Maybe String
  }

type Codegen = ReaderT Env (Writer (Array PsImport))

runCodegen :: forall a. Env -> Codegen a -> Tuple a (Array PsImport)
runCodegen env c = runWriter (runReaderT c env)

emit :: forall m a. MonadTell (Array PsImport) m => PsImport -> a -> m a
emit imp a = map (const a) (tell [ imp ])

oneModule :: String -> Array Module -> Module -> Box
oneModule defaultModulePrefix all mod@(Module name decls annots) = vsep 1 Boxes.left do
  let env = { defaultPrefix: defaultModulePrefix, currentModule: mod, allModules: all }
      Tuple body imports = runCodegen env (traverse typeDecl decls)
  text ("module " <> modulePrefix defaultModulePrefix annots <> "." <> name <> " where")
    : vcat Boxes.left ((renderImports <<< mergeImports $ imports) <#> \i -> text ("import " <> i))
    : body

renderImports :: Array PsImport -> Array String
renderImports =
  map \{ mod, typ, alias } ->
    mod
      <> (fromMaybe "" (typ <#> (\t -> " (" <> t <> ")")))
      <> (fromMaybe "" (alias <#> (" as " <> _)))

mergeImports :: Array PsImport -> Array PsImport
mergeImports imps =
  let
    sorted = Array.sortBy ((compare `on` _.mod) <> (compare `on` _.alias)) imps
    grouped = Array.groupBy (\a b -> a.mod == b.mod && b.alias == b.alias) sorted
  in grouped <#> \group -> do
    let typs = map _.typ group
    { mod: (NonEmptyArray.head group).mod
    , alias: (NonEmptyArray.head group).alias
    , typ:
        if any isNothing typs
          then Nothing
          else Just $ intercalate ", " (Array.nub <<< compact <<< NonEmptyArray.toArray $ typs)
    }

outputSpec :: String -> Array Module -> OutputSpec
outputSpec defaultModulePrefix modules =
  { render: render <<< oneModule defaultModulePrefix modules
  , filePath: \(Module n _ an) ->
      let path = String.replaceAll
                    (String.Pattern ".")
                    (String.Replacement "/")
                    (modulePrefix defaultModulePrefix an)
      in path <> "/" <> n <> ".purs"
  }

modulePrefix :: String -> Annotations -> String
modulePrefix defaultPrefix annots =
  fromMaybe defaultPrefix (Annotations.field "purs" "modulePrefix" annots)

primitive :: Primitive -> Codegen Box
primitive = case _ of
  PBoolean -> pure (text "Boolean")
  PInt -> pure (text "Int")
  PDecimal -> emit { mod: "Data.Decimal", typ: Just "Decimal", alias: Nothing } (text "Decimal")
  PString -> pure (text "String")

type Extern = { prefix :: String, t :: String}

externalType :: Extern -> Codegen Box
externalType { prefix, t } =
  emit { mod: prefix, typ: Just t, alias: Nothing } $ text t

importModule :: String -> String -> PsImport
importModule package mod =
  { mod: package, typ: Nothing, alias: Just mod }

splitType :: String -> Maybe Extern
splitType s = do
  i <- String.lastIndexOf (Pattern ".") s
  let prefix = String.take i s
  let t = String.drop (i + 1) s
  pure $ { prefix, t }

typeDecl :: TypeDecl -> Codegen Box
typeDecl (TypeDecl name tt annots) =
  let dec kw = text kw <<+>> text name <<+>> char '='
  in case tt of
    Type t -> do
      ty <- tyType t
      j <- jsonCodec t
      pure $ (dec "type" <<+>> ty)
        // defJsonCodec name j
    Wrap t ->
      case getWrapOpts "purs" annots of
        Nothing -> do
          other <- otherInstances name
          ty <- tyType t
          j <- newtypeJsonCodec name t
          newtype_ <- newtypeInstances name
          pure $
            dec "newtype" <<+>> text name <<+>> ty
              // newtype_
              // other
              // defJsonCodec name j
        Just { typ, decode, encode } -> do
          ty <- externalRef typ
          j <- externalJsonCodec name t decode encode
          pure $
            dec "type" <<+>> ty
              // defJsonCodec name j
    Record props -> do
      recordDecl <- record props <#> \p -> dec "type" // indented p
      codec <- recordJsonCodec props
      pure $
        recordDecl
          // defJsonCodec name codec
    Sum vs -> do
      other <- otherInstances name
      codec <- sumJsonCodec name vs
      pure $
        dec "data"
          // indented (hsep 1 Boxes.bottom $ vcat Boxes.left <$> [ Array.drop 1 vs <#> \_ -> char '|',  vs <#> text ])
          // other
          // defJsonCodec name codec

sumJsonCodec :: String -> Array String -> Codegen Box
sumJsonCodec name vs = do
  tell
    [ { mod: "Data.Either", typ: Just "Either(..)", alias: Nothing }
    ]
  let encode = text "encode: case _ of"
                // indented (branches encodeBranch)
      decode = text "decode: case _ of"
                // indented (branches decodeBranch // fallthrough)
  pure $ text "Runtime.composeCodec"
          // indented (delimitedLiteral Vert '{' '}' [ decode, encode ] // text "Runtime.jsonCodec_string")
  where
    branches branch = vcat Boxes.left (vs <#> branch)
    encodeBranch v = text v <<+>> text "->" <<+>> text (show v)
    decodeBranch v = text (show v) <<+>> text "-> Right" <<+>> text v
    fallthrough = text $ "s -> Left $ \"Invalid value \" <> show s <> \" for " <> name <> "\""

newtypeInstances :: String -> Codegen Box
newtypeInstances name =
  emit
    { mod: "Data.Newtype", typ: Just "class Newtype", alias: Nothing }
    $ text ("derive instance newtype" <> name <> " :: Newtype " <> name <> " _")

otherInstances :: String -> Codegen Box
otherInstances name = do
  tell
    [ { mod: "Prelude", typ: Nothing, alias: Nothing }
    , { mod: "Data.Generic.Rep", typ: Just "class Generic", alias: Nothing }
    , { mod: "Data.Generic.Rep.Show", typ: Just "genericShow", alias: Nothing }
    ]
  pure $
    text ("derive instance eq" <> name <> " :: Eq " <> name)
      // text ("derive instance ord" <> name <> " :: Ord " <> name)
      // text ("derive instance generic" <> name <> " :: Generic " <> name <> " _")
      // text ("instance show" <> name <> " :: Show " <> name <> " where")
      // indented (text "show a = genericShow a")

tyType :: Type -> Codegen Box
tyType =
  let
    wrap tycon t =
      tyType t <#> \ty -> text tycon <<+>> parens ty
  in case _ of
    Primitive p -> primitive p
    Ref _ { mod, typ } -> do
      { defaultPrefix, allModules } <- ask
      let prefix = mod <#> \m -> fromMaybe defaultPrefix (modPrefix allModules m)
      for_ (lift2 importModule prefix mod) \s -> tell [ s ]
      pure $ text (maybe "" (_ <> ".") mod <> typ)
    Array t -> wrap "Array" t
    Option t -> tell (pure { mod: "Data.Maybe", typ: Just "Maybe", alias: Nothing }) >>= const (wrap "Maybe" t)

modPrefix :: Array Module -> String -> Maybe String
modPrefix all modName = do
  Module _ _ annots <- Array.find (\(Module n _ _) -> n == modName) all
  Annotations.field "purs" "modulePrefix" annots

externalRef :: String -> Codegen Box
externalRef s = fromMaybe (text s # pure) (splitType s <#> externalType)

emitRuntime :: Box -> Codegen Box
emitRuntime b = emit { mod: "Ccap.Codegen.Runtime", typ: Nothing, alias: Just "Runtime" } b

newtypeJsonCodec :: String -> Type -> Codegen Box
newtypeJsonCodec name t = do
  i <- jsonCodec t
  emitRuntime $ text "Runtime.codec_newtype" <<+>> parens i

externalJsonCodec :: String -> Type -> String -> String -> Codegen Box
externalJsonCodec name t decode encode = do
  i <- jsonCodec t
  decode_ <- externalRef decode
  encode_ <- externalRef encode
  emitRuntime $ text "Runtime.codec_custom" <<+>> decode_ <<+>> encode_ <<+>> parens i

codecName :: Maybe String -> String -> String
codecName mod t =
  maybe "" (_ <> ".") mod <> "jsonCodec_" <> t

jsonCodecS :: Type -> String
jsonCodecS ty =
  case ty of
    Primitive p -> codecName (Just "Runtime") (
      case p of
        PBoolean -> "boolean"
        PInt -> "int"
        PDecimal -> "decimal"
        PString -> "string"
      )
    Array t -> tycon "array" t
    Option t -> tycon "maybe" t
    Ref _ { mod, typ } -> codecName mod typ
  where
    tycon which t =
      "(" <> codecName (Just "Runtime") which <> " " <> jsonCodecS t <> ")"

jsonCodec :: Type -> Codegen Box
jsonCodec = jsonCodecS >>> text >>> pure

parens :: Box -> Box
parens b = char '(' <<>> b <<>> char ')'

defJsonCodec :: String -> Box -> Box
defJsonCodec name def =
  let cname = codecName Nothing name
  in text cname <<+>> text ":: Runtime.JsonCodec" <<+>> text name
     // (text cname <<+>> char '=')
     // indented def

record :: Array RecordProp -> Codegen Box
record props = do
  tell [ { mod: "Data.Tuple", typ: Just "Tuple(..)", alias: Nothing } ]
  types <- (\(RecordProp _ t) -> tyType t) `traverse` props
  let labels = props <#> \(RecordProp name _) -> text name <<+>> text "::"
  pure $ delimitedLiteral Vert '{' '}' (Array.zip labels types <#> \(Tuple l t) -> l <<+>> t)

recordJsonCodec :: Array RecordProp -> Codegen Box
recordJsonCodec props = do
  tell
    [ { mod: "Data.Argonaut.Core", typ: Nothing, alias: Just "Argonaut" }
    , { mod: "Ccap.Codegen.Runtime", typ: Nothing, alias: Just "Runtime" }
    , { mod: "Foreign.Object", typ: Nothing, alias: Just "FO" }
    , { mod: "Prelude", typ: Nothing, alias: Nothing }
    ]
  encodeProps <- recordWriteProps props
  decodeProps <- recordReadProps props
  let encode = text "encode: \\p -> Argonaut.fromObject $"
                // indented
                      (text "FO.fromFoldable"
                        // indented encodeProps)
      names = props <#> \(RecordProp name _) -> text name
      decode = text "decode: \\j -> do"
              // indented
                    (text "o <- Runtime.obj j"
                      // decodeProps
                      // (text "pure" <<+>> delimitedLiteral Horiz '{' '}' names))
  pure $ delimitedLiteral Vert '{' '}' [ decode, encode ]

recordWriteProps :: Array RecordProp -> Codegen Box
recordWriteProps props = do
  types <- for props (\(RecordProp name t) -> do
    x <- jsonCodec t
    pure $ text "Tuple" <<+>> text (show name) <<+>> parens (x <<>> text ".encode p." <<>> text name))
  pure $ delimitedLiteral Vert '[' ']' types

recordReadProps :: Array RecordProp -> Codegen Box
recordReadProps props = do
  lines <- for props (\(RecordProp name t) -> do
    x <- jsonCodec t
    pure $ text name <<+>> text "<- Runtime.decodeProperty"
              <<+>> text (show name) <<+>> x <<+>> text "o")
  pure $ vcat Boxes.left lines
