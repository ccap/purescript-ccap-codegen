module Ccap.Codegen.Env
  ( Env
  , askModule
  , askTypeDecl
  , lookupModule
  , lookupTypeDecl
  , traverseM
  , forM
  ) where

import Prelude
import Ccap.Codegen.Types (Module, ModuleName, TypeDecl, typeDeclName)
import Control.Monad.Reader (ReaderT, asks)
import Data.Foldable (find)
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable, traverse)

type Env
  = { allModules :: Array Module
    , currentModule :: Module
    , defaultPrefix :: Maybe String
    }

askModule :: forall m. Monad m => ModuleName -> ReaderT Env m (Maybe Module)
askModule moduleName = lookupModule moduleName <$> asks _.allModules

lookupModule :: ModuleName -> Array Module -> Maybe Module
lookupModule moduleName = find (eq moduleName <<< _.name)

askTypeDecl :: forall m. Monad m => ModuleName -> String -> ReaderT Env m (Maybe TypeDecl)
askTypeDecl moduleName typeName = (lookupTypeDecl typeName =<< _) <$> askModule moduleName

lookupTypeDecl :: String -> Module -> Maybe TypeDecl
lookupTypeDecl typeName = find (eq typeName <<< typeDeclName) <<< _.types

-- **Should be moved to a traverse-extra package **
-- | `traverse` followed by a monadic join inside
traverseM :: forall f g a b. Applicative g => Traversable f => Bind f => (a -> g (f b)) -> f a -> g (f b)
traverseM f = map join <<< traverse f

-- | flipped traverseM
forM :: forall f g a b. Applicative g => Traversable f => Bind f => f a -> (a -> g (f b)) -> g (f b)
forM = flip traverseM
