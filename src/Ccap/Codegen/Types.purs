module Ccap.Codegen.Types
  ( Annotation(..)
  , Annotations
  , AnnotationParam(..)
  , Exports
  , Import
  , Module
  , ModuleName
  , Primitive(..)
  , Type(..)
  , TRef
  , RecordProp(..)
  , Source
  , TopType(..)
  , TypeDecl(..)
  , ValidatedModule
  , Variant
  , isRecord
  , typeDeclName
  , typeDeclTopType
  ) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Node.Path (FilePath)
import Prelude (class Eq, class Show)
import Text.Parsing.Parser.Pos (Position)

type Source a
  = { source :: FilePath
    , contents :: a
    }

type Module
  = { name :: ModuleName
    , types :: Array TypeDecl
    , annots :: Annotations
    , imports :: Array Import
    , exports :: Exports
    }

type ValidatedModule
  = { name :: ModuleName
    , types :: Array TypeDecl
    , annots :: Annotations
    , imports :: Array Module
    , exports :: Exports
    }

type ModuleName
  = String

-- TODO: newtype Import (to distinguish between the import statement and the actual file)
type Import
  = String

--| package names for generating imports from a tmpl file
type Exports
  = { scalaPkg :: String
    , pursPkg :: String
    , tmplPath :: String -- What is the purpose of this?
    }

data TypeDecl
  = TypeDecl String TopType Annotations

type Annotations
  = Array Annotation -- TODO: Consider using a Map?

data Annotation
  = Annotation String Position (Array AnnotationParam)

data AnnotationParam
  = AnnotationParam String Position (Maybe String)

data TopType
  = Type Type
  | Wrap Type
  | Record (Array RecordProp)
  | Sum (Array Variant)

isRecord :: TopType -> Boolean
isRecord (Record _) = true

isRecord _ = false

data Type
  = Primitive Primitive
  | Ref Position TRef
  | Array Type
  | Option Type

type TRef
  = { mod :: Maybe ModuleName, typ :: String }

type RecordProp
  = { name :: String
    , typ :: Type
    , annots :: Annotations
    }

type Variant
  = String

data Primitive
  = PBoolean
  | PInt
  | PDecimal
  | PString

-- Instances here to avoid cluttering the above
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

derive instance eqPrimitive :: Eq Primitive

derive instance genericPrimitive :: Generic Primitive _

instance showPrimitive :: Show Primitive where
  show = genericShow

-- | Get the type name of a type declaration.
typeDeclName :: TypeDecl -> String
typeDeclName (TypeDecl typeName _ _) = typeName

-- | Get the top most type of a type declaration.
typeDeclTopType :: TypeDecl -> TopType
typeDeclTopType (TypeDecl _ topType _) = topType
