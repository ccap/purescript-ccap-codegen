module Ccap.Codegen.Config
  ( Config
  , Mode(..)
  , config
  ) where

import Prelude
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..))
import Data.String (split) as String
import Options.Applicative as OptParse
import Options.Applicative.Types as OptParse.Types

data Mode
  = Pretty
  | Purs
  | PursJs
  | Scala
  | Show
  | Test

type Config
  = { mode :: Mode
    , includes :: Array String
    , outputDirectory :: Maybe String
    , targetFiles :: Array String
    }

config :: OptParse.Parser Config
config = do
  (\mode includes outputDirectory targetFiles ->
     { mode: mode
     , includes: fromMaybe [] includes
     , outputDirectory
     , targetFiles
     }
   )
   <$> modeOption
   <*> includesOption
   <*> outputOption
   <*> filesOption

modeOption :: OptParse.Parser Mode
modeOption =
  let
    readMode :: String -> Either String Mode
    readMode = case _ of
      "pretty" -> Right Pretty
      "purs" -> Right Purs
      "pursjs" -> Right PursJs
      "scala" -> Right Scala
      "show" -> Right Show
      "test" -> Right Test
      m -> Left $ "Unknown mode " <> show m

    modeParse :: OptParse.ReadM Mode
    modeParse = OptParse.eitherReader readMode
  in
  OptParse.option modeParse
    $ fold
        [ OptParse.long "mode"
        , OptParse.metavar "Mode"
        , OptParse.short 'm'
        , OptParse.help "The output mode (must be one of pretty, purs, pursjs, scala, show, or test)"
        ]

filesOption :: OptParse.Parser (Array String)
filesOption =
  map Array.fromFoldable
    $ OptParse.many
    $ OptParse.argument OptParse.str (OptParse.metavar "Target...")

outputOption :: OptParse.Parser (Maybe String)
outputOption =
  OptParse.Types.optional
     $ OptParse.strOption
     $ fold
        [ OptParse.long "output-directory"
        , OptParse.metavar "Output directory"
        , OptParse.short 'o'
        , OptParse.help "Files will be written to this directory"
        ]

includesOption :: OptParse.Parser (Maybe (Array String))
includesOption =
  let
    split = String.split (Pattern ",")

    arrayStringParse = map split OptParse.str
  in
   OptParse.Types.optional
    $ OptParse.option arrayStringParse
    $ fold
        [ OptParse.long "include"
        , OptParse.metavar "Included templates"
        , OptParse.short 'I'
        , OptParse.help "Template definitions to include in scope"
        ]
