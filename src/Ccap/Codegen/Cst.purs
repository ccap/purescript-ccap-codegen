module Ccap.Codegen.Cst
  ( Annotation(..)
  , AnnotationParam(..)
  , Constructor(..)
  , ConstructorName(..)
  , Exports
  , Import(..)
  , Module
  , ModuleName(..)
  , ModuleRef(..)
  , Primitive(..)
  , RecordProp(..)
  , Source
  , TRef
  , TopType(..)
  , Type(..)
  , TypeDecl(..)
  , TypeOrParam(..)
  , TypeParam(..)
  , isRecord
  , topTypeReferences
  , typeDeclName
  , typeDeclTopType
  ) where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Node.Path (FilePath)
import Parsing (Position)

type Source a
  = { source :: FilePath
    , contents :: a
    }

newtype ModuleName
  = ModuleName String

type Module
  = { types :: NonEmptyArray TypeDecl
    , imports :: Array Import
    , exports :: Exports
    , name :: ModuleName
    }

newtype ModuleRef
  = ModuleRef String

data Import
  = Import Position String

--| package names for generating imports from a tmpl file
type Exports
  = { scalaPkg :: String
    , pursPkg :: String
    }

newtype TypeDecl
  = TypeDecl
  { position :: Position
  , name :: String
  , topType :: TopType
  , annots :: Array Annotation
  , params :: Array TypeParam
  }

data Annotation
  = Annotation String Position (Array AnnotationParam)

data AnnotationParam
  = AnnotationParam String Position (Maybe String)

newtype TypeParam
  = TypeParam String

data TopType
  = Type Type
  | Wrap Type
  | Record (NonEmptyArray RecordProp)
  | Sum (NonEmptyArray Constructor)

data Type
  = Primitive Primitive
  | Ref Position TRef
  | Array TypeOrParam
  | Option TypeOrParam
  | TypeWithParens Type

data TypeOrParam
  = TType Type
  | TParam TypeParam

type TRef
  = { mod :: Maybe ModuleRef
    , typ :: String
    , params :: Array TypeOrParam
    }

type RecordProp
  = { name :: String
    , typ :: TypeOrParam
    , annots :: Array Annotation
    , position :: Position
    }

newtype ConstructorName
  = ConstructorName String

data Constructor
  = NoArg ConstructorName
  | WithArgs ConstructorName (NonEmptyArray TypeOrParam)

data Primitive
  = PBoolean
  | PInt
  | PDecimal
  | PString
  | PStringValidationHack
  | PJson

isRecord :: TopType -> Boolean
isRecord = case _ of
  Record _ -> true
  _ -> false

-- | Get the type name of a type declaration.
typeDeclName :: TypeDecl -> String
typeDeclName (TypeDecl { name: typeName }) = typeName

-- | Get the top most type of a type declaration.
typeDeclTopType :: TypeDecl -> TopType
typeDeclTopType (TypeDecl { topType }) = topType

-- | Return all type references used in a Declared Type
topTypeReferences :: TopType -> Array TRef
topTypeReferences = case _ of
  Type typ -> typeReferences typ
  Wrap typ -> typeReferences typ
  Record props ->
    NonEmptyArray.toArray props >>= _.typ
      >>> case _ of
          TType typ -> typeReferences typ
          TParam _ -> []
  Sum constructors ->
    NonEmptyArray.toArray constructors
      >>= case _ of
          NoArg _ -> []
          WithArgs _ args ->
            NonEmptyArray.toArray args
              >>= case _ of
                  TType typ -> typeReferences typ
                  TParam _ -> []

-- | Return all type references in any used type.
typeReferences :: Type -> Array TRef
typeReferences = case _ of
  Ref _ tRef -> [ tRef ]
  Primitive _ -> []
  Array (TType typ) -> typeReferences typ
  Array (TParam _) -> []
  Option (TType typ) -> typeReferences typ
  Option (TParam _) -> []
  TypeWithParens typ -> typeReferences typ

-- Instances here to avoid cluttering the above
derive instance eqModuleName :: Eq ModuleName

derive instance genericModuleName :: Generic ModuleName _

instance showModuleName :: Show ModuleName where
  show t = genericShow t

derive instance eqImport :: Eq Import

derive instance ordImport :: Ord Import

derive instance genericImport :: Generic Import _

instance showImport :: Show Import where
  show t = genericShow t

derive instance eqTypeOrParam :: Eq TypeOrParam

derive instance genericTypeOrParam :: Generic TypeOrParam _

instance showTypeOrParam :: Show TypeOrParam where
  show t = genericShow t

derive instance eqConstructorName :: Eq ConstructorName

derive instance genericConstructorName :: Generic ConstructorName _

instance showConstructorName :: Show ConstructorName where
  show t = genericShow t

derive instance eqConstructor :: Eq Constructor

derive instance genericConstructor :: Generic Constructor _

instance showConstructor :: Show Constructor where
  show t = genericShow t

derive instance eqTypeParam :: Eq TypeParam

derive instance genericTypeParam :: Generic TypeParam _

instance showTypeParam :: Show TypeParam where
  show t = genericShow t

derive instance eqModuleRef :: Eq ModuleRef

derive instance ordModuleRef :: Ord ModuleRef

derive instance genericModuleRef :: Generic ModuleRef _

instance showModuleRef :: Show ModuleRef where
  show t = genericShow t

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
