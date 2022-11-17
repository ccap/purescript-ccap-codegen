module Ccap.Codegen.GetSchemaConfig
  ( GetSchemaConfig
  , getSchemaConfig
  ) where

import Prelude
import Data.Foldable (fold)
import Data.Maybe (Maybe)
import Options.Applicative as OptParse
import Options.Applicative.Types as OptParse.Types

type GetSchemaConfig
  = { domains :: Boolean
    , database :: String
    , table :: Maybe String
    , scalaPkg :: String
    , pursPkg :: String
    }

getSchemaConfig :: OptParse.Parser GetSchemaConfig
getSchemaConfig = do
  ( \domains database table scalaPkg pursPkg ->
      { domains, database, table, scalaPkg, pursPkg }
  )
    <$> domainsOption
    <*> databaseOption
    <*> tableOption
    <*> scalaPkgOption
    <*> pursPkgOption

domainsOption :: OptParse.Parser Boolean
domainsOption =
  OptParse.switch
    $ fold
        [ OptParse.long "domains"
        , OptParse.short 'd'
        ]

databaseOption :: OptParse.Parser String
databaseOption =
  OptParse.strOption
    $ fold
        [ OptParse.long "config"
        , OptParse.metavar "Database"
        , OptParse.short 'c'
        , OptParse.help "The database to use"
        ]

tableOption :: OptParse.Parser (Maybe String)
tableOption =
  OptParse.Types.optional
    $ OptParse.strOption
    $ fold
        [ OptParse.long "table"
        , OptParse.metavar "Table"
        , OptParse.short 't'
        , OptParse.help "Query the provided database table"
        ]

scalaPkgOption :: OptParse.Parser String
scalaPkgOption =
  OptParse.strOption
    $ fold
        [ OptParse.long "scala-pkg"
        , OptParse.metavar "Scala package"
        , OptParse.short 's'
        ]

pursPkgOption :: OptParse.Parser String
pursPkgOption =
  OptParse.strOption
    $ fold
        [ OptParse.long "purs-pkg"
        , OptParse.metavar "Purescript package"
        , OptParse.short 'p'
        ]
