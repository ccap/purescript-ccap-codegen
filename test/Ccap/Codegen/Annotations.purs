module Test.Ccap.Codegen.Annotations
  ( specs
  ) where

import Prelude
import Ccap.Codegen.Cst as Cst
import Ccap.Codegen.PrettyPrint (prettyPrint)
import Ccap.Codegen.Util (scrubEolSpaces)
import Control.Monad.Except (except)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Test.Ccap.Codegen.Util (exceptAffT, parse, runOrFail, sourceCstTmpl)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

tmplFile :: FilePath
tmplFile = "./test/resources/annotations/Annotations.tmpl"

type AnnotParam
  = { name :: String, value :: Maybe String }

type Annot
  = { name :: String, params :: Array AnnotParam }

maxLen5 :: Annot
maxLen5 = { name: "validations", params: [ { name: "maxLength", value: Just "5" } ] }

deposAnnot :: Cst.Annotation -> Annot
deposAnnot (Cst.Annotation name _ params) = { name, params: deposeAnnotParam <$> params }

deposeAnnotParam :: Cst.AnnotationParam -> AnnotParam
deposeAnnotParam (Cst.AnnotationParam name _ value) = { name, value }

findTypeDecl :: String -> NonEmptyArray Cst.TypeDecl -> Maybe Cst.TypeDecl
findTypeDecl typeName = find $ eq typeName <<< Cst.typeDeclName

typeDeclAnnots :: Cst.TypeDecl -> Array Annot
typeDeclAnnots (Cst.TypeDecl { annots }) = annots <#> deposAnnot

typeDeclRecordProps :: Cst.TypeDecl -> Array Cst.RecordProp
typeDeclRecordProps = case _ of
  Cst.TypeDecl { topType: Cst.Record props } -> NonEmptyArray.toArray props
  _ -> []

findRecordProp :: String -> Cst.TypeDecl -> Maybe Cst.RecordProp
findRecordProp propName = find (eq propName <<< _.name) <<< typeDeclRecordProps

recordPropAnnots :: Cst.RecordProp -> Array Annot
recordPropAnnots { annots } = deposAnnot <$> annots

specs :: Spec Unit
specs =
  describe "Annotations" do
    describe "Type declarations can have annotations" do
      it "can be defined inline"
        $ typeShouldHaveAnnotation_ "Inline" maxLen5
      it "can be defined on the next line"
        $ typeShouldHaveAnnotation_ "NextLine" maxLen5
    describe "Record fields can have annotations" do
      it "can be defined inline"
        $ fieldShouldHaveAnnotation_ "Record" "inline" maxLen5
      it "can be defined on the next line"
        $ fieldShouldHaveAnnotation_ "Record" "nextLine" maxLen5
    describe "Annotations on type declarations" do
      describe "when pretty printed" do
        it "keeps inline type annotations"
          $ checkPrint tmplFile
          $ typeShouldHaveAnnotation "Inline" maxLen5
        it "keeps next line type annotations"
          $ checkPrint tmplFile
          $ typeShouldHaveAnnotation "NextLine" maxLen5
        it "keeps inline record prop annotations"
          $ checkPrint tmplFile
          $ fieldShouldHaveAnnotation "Record" "inline" maxLen5
        it "keeps next line record prop annotations"
          $ checkPrint tmplFile
          $ fieldShouldHaveAnnotation "Record" "nextLine" maxLen5

readTypes :: FilePath -> Effect (Either String (NonEmptyArray Cst.TypeDecl))
readTypes = map (map _.contents.types) <<< sourceCstTmpl

typeShouldHaveAnnotation :: String -> Annot -> NonEmptyArray Cst.TypeDecl -> Aff Unit
typeShouldHaveAnnotation typeName annot types =
  let
    annots = maybe [] typeDeclAnnots $ findTypeDecl typeName types
  in
    annots `shouldEqual` [ annot ]

fieldShouldHaveAnnotation :: String -> String -> Annot -> NonEmptyArray Cst.TypeDecl -> Aff Unit
fieldShouldHaveAnnotation typeName propName annot types =
  let
    annots =
      fromMaybe [] do
        typeDecl <- findTypeDecl typeName types
        recordProp <- findRecordProp propName typeDecl
        pure $ recordPropAnnots recordProp
  in
    annots `shouldEqual` [ annot ]

checkTypes :: (NonEmptyArray Cst.TypeDecl -> Aff Unit) -> Aff Unit
checkTypes check = runOrFail $ check <$> (exceptAffT $ readTypes tmplFile)

typeShouldHaveAnnotation_ :: String -> Annot -> Aff Unit
typeShouldHaveAnnotation_ typeName annot = checkTypes $ typeShouldHaveAnnotation typeName annot

fieldShouldHaveAnnotation_ :: String -> String -> Annot -> Aff Unit
fieldShouldHaveAnnotation_ typeName propName annot = checkTypes $ fieldShouldHaveAnnotation typeName propName annot

checkPrint :: FilePath -> (NonEmptyArray Cst.TypeDecl -> Aff Unit) -> Aff Unit
checkPrint filePath check =
  runOrFail do
    source <- exceptAffT $ sourceCstTmpl filePath
    let
      printed = scrubEolSpaces $ prettyPrint source.contents
    resourced <- except $ parse filePath printed
    pure $ check resourced.contents.types
