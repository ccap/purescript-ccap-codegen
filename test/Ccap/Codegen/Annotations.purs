module Test.Ccap.Codegen.Annotations
  ( specs
  ) where

import Prelude
import Ccap.Codegen.PrettyPrint (prettyPrint)
import Ccap.Codegen.Shared (invalidate)
import Ccap.Codegen.Types (Annotation(..), AnnotationParam(..), RecordProp, TopType(..), TypeDecl(..), typeDeclName)
import Ccap.Codegen.Util (scrubEolSpaces)
import Control.Monad.Except (except)
import Data.Either (Either)
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Test.Ccap.Codegen.Util (exceptAffT, parse, runOrFail, sourceTmpl, validateModule)
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

deposAnnot :: Annotation -> Annot
deposAnnot (Annotation name _ params) = { name, params: deposeAnnotParam <$> params }

deposeAnnotParam :: AnnotationParam -> AnnotParam
deposeAnnotParam (AnnotationParam name _ value) = { name, value }

findTypeDecl :: String -> Array TypeDecl -> Maybe TypeDecl
findTypeDecl typeName = find $ eq typeName <<< typeDeclName

typeDeclAnnots :: TypeDecl -> Array Annot
typeDeclAnnots (TypeDecl _ _ annots) = annots <#> deposAnnot

typeDeclRecordProps :: TypeDecl -> Array RecordProp
typeDeclRecordProps (TypeDecl _ (Record props) _) = props

typeDeclRecordProps _ = []

findRecordProp :: String -> TypeDecl -> Maybe RecordProp
findRecordProp propName = find (eq propName <<< _.name) <<< typeDeclRecordProps

recordPropAnnots :: RecordProp -> Array Annot
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

readTypes :: FilePath -> Effect (Either String (Array TypeDecl))
readTypes = map (map _.contents.types) <<< sourceTmpl

typeShouldHaveAnnotation :: String -> Annot -> Array TypeDecl -> Aff Unit
typeShouldHaveAnnotation typeName annot types =
  let
    annots = maybe [] typeDeclAnnots $ findTypeDecl typeName types
  in
    annots `shouldEqual` [ annot ]

fieldShouldHaveAnnotation :: String -> String -> Annot -> Array TypeDecl -> Aff Unit
fieldShouldHaveAnnotation typeName propName annot types =
  let
    annots =
      fromMaybe [] do
        typeDecl <- findTypeDecl typeName types
        recordProp <- findRecordProp propName typeDecl
        pure $ recordPropAnnots recordProp
  in
    annots `shouldEqual` [ annot ]

checkTypes :: (Array TypeDecl -> Aff Unit) -> Aff Unit
checkTypes check = runOrFail $ check <$> (exceptAffT $ readTypes tmplFile)

typeShouldHaveAnnotation_ :: String -> Annot -> Aff Unit
typeShouldHaveAnnotation_ typeName annot = checkTypes $ typeShouldHaveAnnotation typeName annot

fieldShouldHaveAnnotation_ :: String -> String -> Annot -> Aff Unit
fieldShouldHaveAnnotation_ typeName propName annot = checkTypes $ fieldShouldHaveAnnotation typeName propName annot

checkPrint :: FilePath -> (Array TypeDecl -> Aff Unit) -> Aff Unit
checkPrint filePath check =
  runOrFail do
    source <- exceptAffT $ sourceTmpl filePath
    let
      printed = scrubEolSpaces $ prettyPrint (invalidate source.contents)
    resourced <- except $ parse filePath printed
    revalidated <- exceptAffT $ validateModule resourced
    pure $ check revalidated.contents.types
