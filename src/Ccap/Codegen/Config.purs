module Ccap.Codegen.Config
  ( Config
  , Mode(..)
  , config
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String (length, split) as String
import Node.Yargs.Applicative (Y, yarg)

data Mode
  = Pretty
  | Purs
  | Scala
  | Show
  | Test

type Config =
  { mode :: Mode
  , package :: String
  , outputDirectory :: Maybe String
  }

config :: Y (Either String Config)
config = mkConfig <$> yMode <*> yPackage <*> yOutput
  where
    mkConfig mode package outputDirectory =
      mode <#> { mode: _, package, outputDirectory }

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
yOutput = yarg "o" alts desc def true <#> checkString
  where
    alts = [ "output-directory" ]
    desc = Just "Files will be written to this directory"
    def = Left ""
    checkString s = if String.length s > 0 then Just s else Nothing
