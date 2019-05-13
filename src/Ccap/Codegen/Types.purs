module Ccap.Codegen.Types
  ( IsRequired(..)
  , Module(..)
  , Primitive(..)
  , TyTypeNonRecord(..)
  , RecordProp(..)
  , TyType(..)
  , TypeDecl(..)
  ) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Prelude (class Eq, class Show)
import Text.Parsing.Parser.Pos (Position)

data Module = Module String (Array TypeDecl)
derive instance eqModule :: Eq Module
derive instance genericModule :: Generic Module _
instance showModule :: Show Module where
  show = genericShow

data TyTypeNonRecord
  = Primitive Primitive
  | TyRef Position String
  | TyArray TyTypeNonRecord
derive instance eqTyTypeNonRecord :: Eq TyTypeNonRecord
derive instance genericTyTypeNonRecord :: Generic TyTypeNonRecord _
instance showTyTypeNonRecord :: Show TyTypeNonRecord where
  show t = genericShow t

data TyType
  = TyTypeNonRecord TyTypeNonRecord
  | TyRecord (Array RecordProp)
derive instance eqTyType :: Eq TyType
derive instance genericTyType :: Generic TyType _
instance showTyType :: Show TyType where
  show = genericShow

data TypeDecl = TypeDecl String TyType
derive instance eqTypeDecl :: Eq TypeDecl
derive instance genericTypeDecl :: Generic TypeDecl _
instance showTypeDecl :: Show TypeDecl where
  show = genericShow

data RecordProp = RecordProp String TyTypeNonRecord IsRequired
derive instance eqRecordProp :: Eq RecordProp
derive instance genericRecordProp :: Generic RecordProp _
instance showRecordProp :: Show RecordProp where
  show = genericShow

data IsRequired
  = Required
  | Optional
derive instance eqIsRequired :: Eq IsRequired
derive instance genericIsRequired :: Generic IsRequired _
instance showIsRequired :: Show IsRequired where
  show = genericShow

data Primitive
  = PBoolean
  | PDate
  | PDateTime
  | PInt
  | PDecimal
  | PString
  | PTime
derive instance eqPrimitive :: Eq Primitive
derive instance genericPrimitive :: Generic Primitive _
instance showPrimitive :: Show Primitive where
  show = genericShow
