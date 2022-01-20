module Ccap.Codegen.Shared
  ( DelimitedLiteralDir(..)
  , FastPathDecoderType(..)
  , OutputSpec
  , delimitedLiteral
  , fastPathDecoderType
  , indented
  ) where

import Prelude
import Ccap.Codegen.Annotations as Annotations
import Ccap.Codegen.Ast as Ast
import Ccap.Codegen.Cst as Cst
import Data.Array as Array
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Text.PrettyPrint.Boxes (Box, char, emptyBox, hcat, vcat, (<<+>>), (<<>>))
import Text.PrettyPrint.Boxes (left, top) as Boxes

type OutputSpec
  = { render :: Ast.Module -> Maybe String
    , filePath :: Ast.Module -> String
    }

indent :: Box
indent = emptyBox 0 2

indented :: Box -> Box
indented = (<<>>) indent

data DelimitedLiteralDir
  = Vert
  | Horiz

delimitedLiteral ::
  DelimitedLiteralDir ->
  Char ->
  Char ->
  Array Box ->
  Box
delimitedLiteral dir l r boxes =
  let
    all =
      (Array.take 1 boxes <#> (char l <<+>> _))
        <> (Array.drop 1 boxes <#> (char ',' <<+>> _))
  in
    case dir of
      Vert -> vcat Boxes.left (all <> [ char r ])
      Horiz -> hcat Boxes.top (all <> [ char ' ' <<>> char r ])

data FastPathDecoderType
  = FBoolean
  | FInt
  | FDecimal
  | FString
  | FJson
  | FOptionBoolean
  | FOptionInt
  | FOptionDecimal
  | FOptionString
  | FOptionJson

fastPathDecoderType :: Ast.Type -> Maybe FastPathDecoderType
fastPathDecoderType = case _ of
  Ast.Primitive p -> Just (cstPrimitiveToDecoderType p)
  Ast.Ref { decl: Just (Tuple _ (Ast.TypeDecl { topType: Ast.Wrap tt, annots })) }
    | isNothing (Annotations.getWrapOpts "purs" annots) -> fastPathDecoderType tt
  Ast.Option (Ast.TType (Ast.Primitive p)) -> Just (cstPrimitiveToOptionDecoderType p)
  Ast.Option (Ast.TType (Ast.Ref { decl: Just (Tuple _ (Ast.TypeDecl { topType: Ast.Wrap tt, annots: annots })) }))
    | isNothing (Annotations.getWrapOpts "purs" annots) -> case fastPathDecoderType tt of
      Just FBoolean -> Just FOptionBoolean
      Just FInt -> Just FOptionInt
      Just FDecimal -> Just FOptionDecimal
      Just FString -> Just FOptionString
      Just FJson -> Just FOptionJson
      _ -> Nothing
  _ -> Nothing
  where
  cstPrimitiveToDecoderType :: Cst.Primitive -> FastPathDecoderType
  cstPrimitiveToDecoderType = case _ of
    Cst.PBoolean -> FBoolean
    Cst.PInt -> FInt
    Cst.PDecimal -> FDecimal
    Cst.PString -> FString
    Cst.PStringValidationHack -> FString
    Cst.PJson -> FJson

  cstPrimitiveToOptionDecoderType :: Cst.Primitive -> FastPathDecoderType
  cstPrimitiveToOptionDecoderType = case _ of
    Cst.PBoolean -> FOptionBoolean
    Cst.PInt -> FOptionInt
    Cst.PDecimal -> FOptionDecimal
    Cst.PString -> FOptionString
    Cst.PStringValidationHack -> FOptionString
    Cst.PJson -> FOptionJson
