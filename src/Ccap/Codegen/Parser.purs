module Ccap.Codegen.Parser
  ( Error(..)
  , errorMessage
  , parseSource
  , roundTrip
  , wholeFile
  ) where

import Prelude
import Ccap.Codegen.Cst as Cst
import Ccap.Codegen.PrettyPrint as PrettyPrint
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode (isLower)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Identity (Identity)
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty ((:|))
import Data.String.CodeUnits (fromCharArray, singleton) as SCU
import Data.String.CodePoints as CodePoints
import Node.Path (FilePath)
import Node.Path as Path
import Parsing (Parser, ParserT, Position(..), position, runParser)
import Parsing as Parser
import Parsing.Combinators (option, optional, try, (<?>))
import Parsing.Language (javaStyle)
import Parsing.String (char, satisfy)
import Parsing.Token (GenLanguageDef(..), GenTokenParser, alphaNum, makeTokenParser, unGenLanguageDef, upper)

tokenParser :: GenTokenParser String Identity
tokenParser =
  makeTokenParser
    $ LanguageDef
        (unGenLanguageDef javaStyle)
          { identStart = lower
          , identLetter = alphaNum
          , reservedNames =
            [ "scala"
            , "purs"
            , "Boolean"
            , "Int"
            , "Decimal"
            , "String"
            , "StringValidationHack"
            , "Array"
            , "Maybe"
            , "wrap"
            , "import"
            , "type"
            ]
          }

stringLiteral :: Parser String String
stringLiteral = tokenParser.stringLiteral

reserved :: String -> Parser String Unit
reserved = tokenParser.reserved

braces :: forall a. Parser String a -> Parser String a
braces = tokenParser.braces

brackets :: forall a. Parser String a -> Parser String a
brackets = tokenParser.brackets

-- | Parse phrases prefixed by a separator, requiring at least one match.
startBy1 :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
startBy1 p sep = sep *> sepBy1Nel p sep

pipeSep1 :: forall a. Parser String a -> Parser String (NonEmptyArray a)
pipeSep1 a = (a `startBy1` (lexeme $ char '|')) <#> NonEmptyArray.fromFoldable1

whiteSpace :: Parser String Unit
whiteSpace = tokenParser.whiteSpace

lower :: Parser String Char
lower = satisfy (isLower <<< CodePoints.codePointFromChar) <?> "lowercase letter"

identifier :: Parser String String
identifier = tokenParser.identifier

lexeme :: forall a. Parser String a -> Parser String a
lexeme = tokenParser.lexeme

importOrTypeName :: Parser String String
importOrTypeName = lexeme $ mkImportOrTypeName <$> upper <*> Array.many alphaNum
  where
  mkImportOrTypeName :: Char -> Array Char -> String
  mkImportOrTypeName c s = SCU.singleton c <> SCU.fromCharArray s

packageName :: Parser String String
packageName = lexeme $ Array.many (alphaNum <|> char '.') <#> SCU.fromCharArray

typeOrParam :: Unit -> Parser String Cst.TypeOrParam
typeOrParam _ = map Cst.TType (tyTypeWithParens unit) <|> map (Cst.TParam <<< Cst.TypeParam) identifier

tRef :: Unit -> Parser String Cst.TRef
tRef _ = do
  parts <- importOrTypeName `sepBy1Nel` char '.'
  params <- Array.many (typeOrParam unit)
  let
    { init, last: typ } = NonEmpty.unsnoc parts

    mod = if init == Nil then Nothing else Just $ intercalate "." init
  pure { mod: map Cst.ModuleRef mod, typ, params }

primitive :: String -> Cst.Primitive -> Parser String Cst.Typ
primitive s decl = reserved s <#> const (Cst.Primitive decl)

anyPrimitiveExceptJson :: Parser String Cst.Typ
anyPrimitiveExceptJson =
  primitive "Boolean" Cst.PBoolean
    <|> primitive "Int" Cst.PInt
    <|> primitive "Decimal" Cst.PDecimal
    <|> primitive "String" Cst.PString
    <|> primitive "StringValidationHack" Cst.PStringValidationHack

tyTypeWithParens :: Unit -> Parser String Cst.Typ
tyTypeWithParens _ = lexeme (char '(') *> map Cst.TypeWithParens (tyType unit) <* lexeme (char ')') <|> tyType unit

tyType :: Unit -> Parser String Cst.Typ
tyType _ =
  anyPrimitiveExceptJson
    <|> (reserved "Array" >>= typeOrParam <#> Cst.Array)
    <|> (reserved "Maybe" >>= typeOrParam <#> Cst.Option)
    <|> (Cst.Ref <$> position <*> tRef unit)

topType :: Unit -> Parser String Cst.TopType
topType _ =
  (tyTypeWithParens unit <#> Cst.Typ)
    <|> (braces $ many1 recordProp <#> Cst.Record)
    <|> (brackets $ pipeSep1 (constructor unit) <#> Cst.Sum)
    <|> (reserved "wrap" >>= (\_ -> primitive "Json" Cst.PJson <|> tyTypeWithParens unit) <#> Cst.Wrap)

constructor :: Unit -> Parser String Cst.Constructor
constructor _ = ado
  name <- map Cst.ConstructorName importOrTypeName
  params <- Array.many (typeOrParam unit)
  in maybe (Cst.NoArg name) (Cst.WithArgs name) (NonEmptyArray.fromArray params)

recordProp :: Parser String Cst.RecordProp
recordProp = ado
  p <- position
  name <- identifier
  lexeme $ char ':'
  typ <- typeOrParam unit
  annots <- Array.many annotation
  try (optional (lexeme (char ',')))
  in { name, typ, annots, position: p }

exports :: Parser String Cst.Exports
exports = ado
  reserved "scala"
  lexeme $ char ':'
  scalaPkg <- lexeme $ packageName
  reserved "purs"
  lexeme $ char ':'
  pursPkg <- lexeme $ packageName
  in { scalaPkg, pursPkg }

imports :: Parser String (Array Cst.Import)
imports =
  Array.many do
    p <- position
    reserved "import"
    n <- packageName
    pure (Cst.Import p n)

oneModule :: Cst.ModuleName -> Parser String Cst.Module
oneModule name = ado
  expts <- exports
  imprts <- imports
  types <- many1 typeDecl
  in { types, imports: imprts, exports: expts, name }

typeDecl :: Parser String Cst.TypeDecl
typeDecl = do
  reserved "type"
  p <- position
  name <- importOrTypeName
  params <- map (map Cst.TypeParam) (Array.many identifier)
  _ <- lexeme $ char ':'
  ty <- topType unit
  annots <- Array.many annotation
  pure (Cst.TypeDecl { position: p, name, topType: ty, annots, params })

annotation :: Parser String Cst.Annotation
annotation = ado
  pos <- position
  lexeme $ char '<'
  name <- (reserved "scala" *> pure "scala") <|> (reserved "purs" *> pure "purs") <|> identifier
  params <- Array.many annotationParam
  lexeme $ char '>'
  in Cst.Annotation name pos params

annotationParam :: Parser String Cst.AnnotationParam
annotationParam = ado
  pos <- position
  name <- identifier
  value <- option Nothing (lexeme (char '=') *> stringLiteral <#> Just)
  in Cst.AnnotationParam name pos value

wholeFile :: Cst.ModuleName -> Parser String Cst.Module
wholeFile name = whiteSpace *> oneModule name

data Error
  = Error FilePath Position String

parseSource :: FilePath -> String -> Either Error (Cst.Source Cst.Module)
parseSource filePath contents = do
  let
    name = Path.basenameWithoutExt filePath ".tmpl"
  lmap
    (parseErrorToError filePath)
    ( runParser contents (wholeFile (Cst.ModuleName name))
        <#> \mod ->
            { source: filePath
            , contents: mod
            }
    )

parseErrorToError :: FilePath -> Parser.ParseError -> Error
parseErrorToError filePath e = Error filePath (Parser.parseErrorPosition e) (Parser.parseErrorMessage e)

errorMessage :: Error -> String
errorMessage (Error filePath (Position pos) message) = filePath <> ":" <> show pos.line <> ":" <> show pos.column <> " " <> message

roundTrip :: Cst.Source Cst.Module -> Either Error Boolean
roundTrip { source: filePath, contents: module1 } = do
  let
    prettyPrinted1 = PrettyPrint.prettyPrint module1
  module2 <- lmap (parseErrorToError filePath) (runParser prettyPrinted1 (wholeFile module1.name))
  let
    prettyPrinted2 = PrettyPrint.prettyPrint module2 { imports = module1.imports }
  pure $ prettyPrinted1 == prettyPrinted2

-- | Parse phrases delimited by a separator, requiring at least one match.
sepBy1Nel :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepBy1Nel p sep = do
  a <- p
  as <- List.many $ sep *> p
  pure $ NonEmptyList (a :| as)

many1 :: forall m s a. Monad m => ParserT s m a -> ParserT s m (NonEmptyArray a)
many1 p = NonEmptyArray.cons' <$> p <*> Array.many p
