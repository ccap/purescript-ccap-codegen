module Ccap.Codegen.Parser
 ( errorMessage
 , roundTrip
 , wholeFile
 ) where

import Prelude

import Ccap.Codegen.PrettyPrint (prettyPrint) as PrettyPrinter
import Ccap.Codegen.Types (Module(..), Primitive(..), RecordProp(..), TopType(..), Type(..), TypeDecl(..))
import Control.Alt ((<|>))
import Data.Array (fromFoldable, many, some) as Array
import Data.Char.Unicode (isLower)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List)
import Data.String.CodeUnits (fromCharArray, singleton) as String
import Text.Parsing.Parser (ParseError, ParserT, parseErrorMessage, parseErrorPosition, position, runParser)
import Text.Parsing.Parser.Combinators (sepBy1, (<?>))
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (char, satisfy)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, alphaNum, makeTokenParser, unGenLanguageDef, upper)

tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $
  LanguageDef (unGenLanguageDef javaStyle)
    { reservedNames =
        [ "module"
        , "type"
        , "boolean"
        , "date"
        , "dateTime"
        , "decimal"
        , "int"
        , "optional"
        , "string"
        , "time"
        ]
    , identStart = lower
    , identLetter = alphaNum
    }

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
        mkModuleOrTypeName c s = String.singleton c <> String.fromCharArray s

primitive :: String -> Primitive -> ParserT String Identity Type
primitive s decl = reserved s <#> const (Primitive decl)

anyPrimitive :: ParserT String Identity Type
anyPrimitive =
  primitive "boolean" PBoolean
    <|> primitive "date" PDate
    <|> primitive "dateTime" PDateTime
    <|> primitive "int" PInt
    <|> primitive "decimal" PDecimal
    <|> primitive "string" PString
    <|> primitive "time" PTime

tyType :: Unit -> ParserT String Identity Type
tyType _ =
  anyPrimitive
    <|> (Ref <$> position <*> moduleOrTypeName)
    <|> (reserved "array" >>= tyType <#> Array)
    <|> (reserved "optional" >>= tyType <#> Option)

topType :: ParserT String Identity TopType
topType =
  (tyType unit <#> Type)
    <|> (braces $ commaSep1 recordProp <#> Record)
    <|> (brackets $ pipeSep1 moduleOrTypeName <#> Sum)
    <|> (reserved "wrap" >>= tyType <#> Wrap)

recordProp :: ParserT String Identity RecordProp
recordProp = ado
  name <- identifier
  lexeme $ char ':'
  ty <- tyType unit
  in RecordProp name ty

oneModule :: ParserT String Identity Module
oneModule = ado
  reserved "module"
  name <- moduleOrTypeName
  decls <- braces $ commaSep1 typeDecl
  in Module name decls

typeDecl :: ParserT String Identity TypeDecl
typeDecl = ado
  reserved "type"
  name <- moduleOrTypeName
  lexeme $ char ':'
  ty <- topType
  in TypeDecl name ty

wholeFile :: ParserT String Identity (Array Module)
wholeFile = whiteSpace *> Array.some oneModule

errorMessage :: String -> ParseError -> String
errorMessage fileName err =
  let Position pos = parseErrorPosition err
  in
    "ERROR in "
      <> fileName
      <> ": line "
      <> show pos.line
      <> ", column "
      <> show pos.column
      <> ": "
      <> parseErrorMessage err

roundTrip :: String -> Either ParseError Boolean
roundTrip contents = do
  modules1 <- runParser contents wholeFile
  let prettyPrinted1 = PrettyPrinter.prettyPrint modules1
  modules2 <- runParser prettyPrinted1 wholeFile
  let prettyPrinted2 = PrettyPrinter.prettyPrint modules2
  pure $ prettyPrinted1 == prettyPrinted2
