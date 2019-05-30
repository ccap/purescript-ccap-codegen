module Ccap.Codegen.Types
  ( Annotation(..)
  , AnnotationParam(..)
  , Module(..)
  , Primitive(..)
  , Type(..)
  , RecordProp(..)
  , TopType(..)
  , TypeDecl(..)
  , Variant
  , WrapOpts
  ) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Prelude (class Eq, class Show)
import Text.Parsing.Parser.Pos (Position)

data Module = Module String (Array TypeDecl)

data TypeDecl = TypeDecl String TopType (Array Annotation)

data Annotation = Annotation String Position (Array AnnotationParam)

data AnnotationParam = AnnotationParam String Position (Maybe String)

data TopType
  = Type Type
  | Wrap Type (Map String WrapOpts)
  | Record (Array RecordProp)
  | Sum (Array Variant)

data Type
  = Primitive Primitive
  | Ref Position String
  | Array Type
  | Option Type

data RecordProp = RecordProp String Type

type Variant = String

data Primitive
  = PBoolean
  | PInt
  | PDecimal
  | PString

type WrapOpts =
  { typ :: String
  , wrap :: String
  , unwrap :: String
  }

-- Instances here to avoid cluttering the above

derive instance eqModule :: Eq Module
derive instance genericModule :: Generic Module _
instance showModule :: Show Module where
  show = genericShow

derive instance eqType :: Eq Type
derive instance genericType :: Generic Type _
instance showType :: Show Type where
  show t = genericShow t

derive instance eqTopType :: Eq TopType
derive instance genericTopType :: Generic TopType _
instance showTopType :: Show TopType where
  show = genericShow

derive instance eqTypeDecl :: Eq TypeDecl
derive instance genericTypeDecl :: Generic TypeDecl _
instance showTypeDecl :: Show TypeDecl where
  show = genericShow

derive instance eqAnnotation :: Eq Annotation
derive instance genericAnnotation :: Generic Annotation _
instance showAnnotation :: Show Annotation where
  show = genericShow

derive instance eqAnnotationParam :: Eq AnnotationParam
derive instance genericAnnotationParam :: Generic AnnotationParam _
instance showAnnotationParam :: Show AnnotationParam where
  show = genericShow

derive instance eqRecordProp :: Eq RecordProp
derive instance genericRecordProp :: Generic RecordProp _
instance showRecordProp :: Show RecordProp where
  show = genericShow

derive instance eqPrimitive :: Eq Primitive
derive instance genericPrimitive :: Generic Primitive _
instance showPrimitive :: Show Primitive where
  show = genericShow
