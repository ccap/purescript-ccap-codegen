module Ccap.Codegen.Parser
 ( errorMessage
 , roundTrip
 , wholeFile
 ) where

import Prelude

import Ccap.Codegen.PrettyPrint (prettyPrint) as PrettyPrinter
import Ccap.Codegen.Types (Annotation(..), AnnotationParam(..), Imports, Module(..), Primitive(..), RecordProp(..), TRef, TopType(..), Type(..), TypeDecl(..))
import Control.Alt ((<|>))
import Data.Array (fromFoldable, many) as Array
import Data.Char.Unicode (isLower)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Identity (Identity)
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.String.CodeUnits (fromCharArray, singleton) as SCU
import Text.Parsing.Parser (ParseError, ParserT, parseErrorMessage, parseErrorPosition, position, runParser)
import Text.Parsing.Parser.Combinators (option, sepBy1, (<?>))
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (char, satisfy)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, alphaNum, makeTokenParser, unGenLanguageDef, upper)

tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $
  LanguageDef (unGenLanguageDef javaStyle)
    { identStart = lower
    , identLetter = alphaNum
    }

stringLiteral :: ParserT String Identity String
stringLiteral = tokenParser.stringLiteral

reserved :: String -> ParserT String Identity Unit
reserved = tokenParser.reserved

commaSep1 :: forall a.  ParserT String Identity a -> ParserT String Identity (Array a)
commaSep1 inner = tokenParser.commaSep1 inner <#> Array.fromFoldable

braces :: forall a. ParserT String Identity  a -> ParserT String Identity a
braces = tokenParser.braces

brackets :: forall a. ParserT String Identity  a -> ParserT String Identity a
brackets = tokenParser.brackets

-- | Parse phrases prefixed by a separator, requiring at least one match.
startBy1 :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
startBy1 p sep = sep *> sepBy1 p sep

pipeSep1 :: forall a.  ParserT String Identity a -> ParserT String Identity (Array a)
pipeSep1 a = (a `startBy1` (lexeme $ char '|')) <#> Array.fromFoldable

whiteSpace :: ParserT String Identity Unit
whiteSpace = tokenParser.whiteSpace

lower :: ParserT String Identity Char
lower = satisfy isLower <?> "lowercase letter"

identifier :: ParserT String Identity String
identifier = tokenParser.identifier

lexeme :: forall a. ParserT String Identity a -> ParserT String Identity a
lexeme = tokenParser.lexeme

moduleOrTypeName :: ParserT String Identity String
moduleOrTypeName = lexeme $ mkModuleOrTypeName <$> upper <*> Array.many alphaNum
  where mkModuleOrTypeName :: Char -> Array Char -> String
        mkModuleOrTypeName c s = SCU.singleton c <> SCU.fromCharArray s

tRef :: ParserT String Identity TRef
tRef = ado
  parts <- moduleOrTypeName `sepBy1Nel` char '.'
  let { init, last: typ } = NonEmpty.unsnoc parts
  let mod = if init == Nil then Nothing else Just $ intercalate "." init
  in { mod, typ }

primitive :: String -> Primitive -> ParserT String Identity Type
primitive s decl = reserved s <#> const (Primitive decl)

anyPrimitive :: ParserT String Identity Type
anyPrimitive =
  primitive "Boolean" PBoolean
    <|> primitive "Int" PInt
    <|> primitive "Decimal" PDecimal
    <|> primitive "String" PString

tyType :: Unit -> ParserT String Identity Type
tyType _ =
  anyPrimitive
    <|> (reserved "Array" >>= tyType <#> Array)
    <|> (reserved "Maybe" >>= tyType <#> Option)
    <|> (Ref <$> position <*> tRef)

topType :: ParserT String Identity TopType
topType =
  (tyType unit <#> Type)
    <|> (braces $ Array.many recordProp <#> Record)
    <|> (brackets $ pipeSep1 moduleOrTypeName <#> Sum)
    <|> (reserved "wrap" >>= tyType <#> Wrap)

recordProp :: ParserT String Identity RecordProp
recordProp = ado
  name <- identifier
  lexeme $ char ':'
  ty <- tyType unit
  in RecordProp name ty

imports :: ParserT String Identity Imports
imports = Array.many
  do
    reserved "import"
    moduleOrTypeName

oneModule :: ParserT String Identity Module
oneModule = ado
  reserved "module"
  name <- moduleOrTypeName
  imprts <- imports
  annots <- Array.many annotation
  lexeme $ char '{'
  decls <- Array.many typeDecl
  lexeme $ char '}'
  in Module name decls annots imprts

typeDecl :: ParserT String Identity TypeDecl
typeDecl = ado
  reserved "type"
  name <- moduleOrTypeName
  lexeme $ char ':'
  ty <- topType
  annots <- Array.many annotation
  in TypeDecl name ty annots

annotation :: ParserT String Identity Annotation
annotation = ado
  pos <- position
  lexeme $ char '<'
  name <- identifier
  params <- Array.many annotationParam
  lexeme $ char '>'
  in Annotation name pos params

annotationParam :: ParserT String Identity AnnotationParam
annotationParam = ado
  pos <- position
  name <- identifier
  value <- option Nothing (lexeme (char '=') *> stringLiteral <#> Just)
  in AnnotationParam name pos value

wholeFile :: ParserT String Identity Module
wholeFile = whiteSpace *> oneModule

errorMessage :: String -> ParseError -> String
errorMessage fileName err =
  let Position pos = parseErrorPosition err
  in
    "Could not parse "
      <> fileName
      <> ": line "
      <> show pos.line
      <> ", column "
      <> show pos.column
      <> ": "
      <> parseErrorMessage err

roundTrip :: Module -> Either ParseError Boolean
roundTrip module1 = do
  let prettyPrinted1 = PrettyPrinter.prettyPrint module1
  module2 <- runParser prettyPrinted1 wholeFile
  let prettyPrinted2 = PrettyPrinter.prettyPrint module2
  pure $ prettyPrinted1 == prettyPrinted2


-- TODO: Push this upstream to purescript-parsing?
-- | Parse phrases delimited by a separator, requiring at least one match.
sepBy1Nel :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepBy1Nel p sep = do
  a <- p
  as <- List.many $ sep *> p
  pure $ NonEmptyList (a :| as)
