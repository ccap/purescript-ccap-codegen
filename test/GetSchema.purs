module Test.GetSchema where

import Prelude
import Ccap.Codegen.Database (tableModule)
import Ccap.Codegen.PrettyPrint (prettyPrint)
import Ccap.Codegen.Shared (invalidate)
import Ccap.Codegen.Types (Source, ValidatedModule, Module)
import Ccap.Codegen.Util (scrubEolSpaces)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either (either)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), uncurry)
import Database.PostgreSQL.PG (PoolConfiguration, defaultPoolConfiguration, newPool)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Node.Process (lookupEnv)
import Test.Ccap.Codegen.Util (diffByLine, sourceTmpl)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] specs

caseTmplFile :: FilePath
caseTmplFile = "./test/resources/get-schema/Case.tmpl"

poolConfig :: Effect PoolConfiguration
poolConfig = do
  host <- lookupEnv "DB_HOST"
  port <- lookupEnv "DB_PORT" <#> (_ >>= Int.fromString)
  db <- lookupEnv "DATABASE"
  user <- lookupEnv "DB_USER"
  password <- lookupEnv "DB_PASSWORD"
  pure
    $ (defaultPoolConfiguration $ fromMaybe "cir" db)
        { host = host
        , port = port
        , user = user
        , password = password
        }

stripImports :: Source ValidatedModule -> Source ValidatedModule
stripImports source = source { contents = source.contents { imports = [] } }

specs :: Spec Unit
specs =
  describe "Case" do
    it "fetches the case data correctly" do
      results <-
        runExceptT do
          fileSource <- ExceptT <<< liftEffect $ sourceTmpl caseTmplFile
          let
            { scalaPkg, pursPkg } = fileSource.contents.exports
          pool <- ExceptT <<< liftEffect <<< map pure $ poolConfig >>= newPool
          dbModule <- tableModule pool scalaPkg pursPkg fileSource.contents.name
          pure $ Tuple (invalidate fileSource.contents) dbModule
      either fail (uncurry printAndDiff) results

printAndDiff :: Module -> Module -> Aff Unit
printAndDiff x y = (print x) `diffByLine` (print y)
  where
  print = scrubEolSpaces <<< prettyPrint
