module Ccap.Codegen.Config
  ( Config
  , Mode(..)
  , config
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..))
import Data.String (split) as String
import Node.Yargs.Applicative (Y, yarg)

data Mode
  = Pretty
  | Purs
  | Scala
  | Show
  | Test

type Config =
  { mode :: Mode
  , includes :: Array String
  , package :: String
  , outputDirectory :: Maybe String
  }

config :: Y (Either String Config)
config = mkConfig <$> yMode <*> yPackage <*> yOutput <*> yIncludes
  where
    mkConfig mode package outputDirectory includes =
      mode <#> { mode: _, package, outputDirectory, includes }

yMode :: Y (Either String Mode)
yMode = yarg "m" alts desc def true <#> readMode
  where
    alts = [ "mode" ]
    desc = Just "The output mode (must be one of pretty, purs, scala, show, or test)"
    def = Right "Mode is required"
    readMode = case _ of
      "pretty" -> Right Pretty
      "purs" -> Right Purs
      "scala" -> Right Scala
      "show" -> Right Show
      "test" -> Right Test
      m -> Left $ "Unknown mode " <> show m

yPackage :: Y String
yPackage = yarg "p" alts desc def true
  where
    alts = [ "package" ]
    desc = Just "The package (Scala) or module prefix (PureScript) to use"
    def = Right "Package is required"

yOutput :: Y (Maybe String)
yOutput = yarg "o" alts desc def true <#> nonEmpty
  where
    alts = [ "output-directory" ]
    desc = Just "Files will be written to this directory"
    def = Left ""

yIncludes :: Y (Array String)
yIncludes = yarg "I" alts desc def false <#> parse
  where
    alts = [ "include" ]
    desc = Just "Template definitions to include in scope"
    def = Left ""
    parse = maybe [] split <<< nonEmpty
    split = String.split (Pattern ",")

-- Data.String.NonEmpty exist if more is needed
nonEmpty :: String -> Maybe String
nonEmpty "" = Nothing
nonEmpty str = Just str
