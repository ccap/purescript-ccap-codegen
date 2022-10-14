module Ccap.Codegen.Ast
  ( Constructor(..)
  , Module(..)
  , RecordProp(..)
  , ScalaDecoderType(..)
  , TRef
  , TopType(..)
  , Typ(..)
  , TypeDecl(..)
  , TypeOrParam(..)
  , isRecord
  , noArgConstructorNames
  , typeDeclName
  , typeDeclTopType
  ) where

import Prelude
import Ccap.Codegen.Cst as Cst
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)

newtype Module
  = Module
  { types :: NonEmptyArray TypeDecl
  , imports :: Array Module
  , exports :: Cst.Exports
  , name :: Cst.ModuleName
  }

newtype TypeDecl
  = TypeDecl
  { name :: String
  , topType :: TopType
  , annots :: Array Cst.Annotation
  , isPrimary :: Boolean
  , params :: Array Cst.TypeParam
  , scalaDecoderType :: Maybe ScalaDecoderType
  }

data TopType
  = Typ Typ
  | Wrap Typ
  | Record (NonEmptyArray RecordProp)
  | Sum (NonEmptyArray Constructor)

data Typ
  = Primitive Cst.Primitive
  | Ref TRef
  | Array TypeOrParam
  | Option TypeOrParam

data TypeOrParam
  = TType Typ
  | TParam Cst.TypeParam

type TRef
  = { decl :: Maybe (Tuple Module TypeDecl)
    , typ :: String
    , params :: Array TypeOrParam
    , isPrimaryRef :: Boolean
    }

type RecordProp
  = { name :: String
    , typ :: TypeOrParam
    , annots :: Array Cst.Annotation
    }

data Constructor
  = NoArg Cst.ConstructorName
  | WithArgs Cst.ConstructorName (NonEmptyArray TypeOrParam)

isRecord :: TopType -> Boolean
isRecord = case _ of
  Record _ -> true
  _ -> false

-- | Get the type name of a type declaration.
typeDeclName :: TypeDecl -> String
typeDeclName (TypeDecl { name }) = name

-- | Get the top most type of a type declaration.
typeDeclTopType :: TypeDecl -> TopType
typeDeclTopType (TypeDecl { topType }) = topType

noArgConstructorNames :: NonEmptyArray Constructor -> Maybe (NonEmptyArray String)
noArgConstructorNames =
  traverse
    ( case _ of
        NoArg (Cst.ConstructorName n) -> Just n
        WithArgs _ _ -> Nothing
    )

data ScalaDecoderType
  = Field
  | Form

-- Instances here to avoid cluttering the above
derive instance eqScalaDecoderType :: Eq ScalaDecoderType

derive instance genericScalaDecoderType :: Generic ScalaDecoderType _

instance showScalaDecoderType :: Show ScalaDecoderType where
  show t = genericShow t

derive instance eqModule :: Eq Module

derive instance genericModule :: Generic Module _

instance showModule :: Show Module where
  show t = genericShow t

derive instance eqTypeOrParam :: Eq TypeOrParam

derive instance genericTypeOrParam :: Generic TypeOrParam _

instance showTypeOrParam :: Show TypeOrParam where
  show t = genericShow t

derive instance eqConstructor :: Eq Constructor

derive instance genericConstructor :: Generic Constructor _

instance showConstructor :: Show Constructor where
  show t = genericShow t

derive instance eqTyp :: Eq Typ

derive instance genericTyp :: Generic Typ _

instance showTyp :: Show Typ where
  show t = genericShow t

derive instance eqTopType :: Eq TopType

derive instance genericTopType :: Generic TopType _

instance showTopType :: Show TopType where
  show = genericShow

derive instance eqTypeDecl :: Eq TypeDecl

derive instance genericTypeDecl :: Generic TypeDecl _

instance showTypeDecl :: Show TypeDecl where
  show = genericShow
