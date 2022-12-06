module Test.GetSchema where

import Prelude
import Ccap.Codegen.Cst as Cst
import Ccap.Codegen.Database (tableModule)
import Ccap.Codegen.PrettyPrint (prettyPrint)
import Ccap.Codegen.Util (scrubEolSpaces)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either (either)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), uncurry)
import Database.PostgreSQL.Pool as Pool
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Node.Process (lookupEnv)
import Test.Ccap.Codegen.Util (diffByLine, sourceCstTmpl)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] specs

caseTmplFile :: FilePath
caseTmplFile = "./test/resources/get-schema/Case.tmpl"

poolConfig :: Effect Pool.Configuration
poolConfig = do
  host <- lookupEnv "DB_HOST"
  port <- lookupEnv "DB_PORT" <#> (_ >>= Int.fromString)
  db <- lookupEnv "DATABASE"
  user <- lookupEnv "DB_USER"
  password <- lookupEnv "DB_PASSWORD"
  pure
    $ (Pool.defaultConfiguration $ fromMaybe "cir" db)
        { host = host
        , port = port
        , user = user
        , password = password
        }

stripImports :: Cst.Source Cst.Module -> Cst.Source Cst.Module
stripImports source = source { contents = source.contents { imports = [] } }

specs :: Spec Unit
specs =
  describe "Case" do
    it "fetches the case data correctly" do
      results <-
        runExceptT do
          fileSource <- ExceptT <<< liftEffect $ sourceCstTmpl caseTmplFile
          let
            { scalaPkg, pursPkg } = fileSource.contents.exports
          pool <- ExceptT <<< liftEffect <<< map pure $ poolConfig >>= Pool.new
          dbModule <-
            tableModule pool
              { scalaPkg
              , pursPkg
              }
              "Case"
          pure $ Tuple fileSource.contents dbModule
      either fail (uncurry printAndDiff) results

printAndDiff :: Cst.Module -> Cst.Module -> Aff Unit
printAndDiff x y = (print x) `diffByLine` (print y)
  where
  print = scrubEolSpaces <<< prettyPrint
