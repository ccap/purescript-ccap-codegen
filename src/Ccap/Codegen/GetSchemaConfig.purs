module Ccap.Codegen.GetSchemaConfig
  ( DomainsConfig
  , SchemaConfig(..)
  , TableConfig
  , getSchemaConfig
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String (split) as String
import Options.Applicative as OptParse
import Options.Applicative.Types as OptParse.Types

data SchemaConfig
  = Table TableConfig
  | Domains DomainsConfig

type TableConfig
  = { database :: String
    , dbManagedColumns :: Maybe (Array String)
    , table :: String
    , pursPkg :: String
    , scalaPkg :: String
    }

type DomainsConfig
  = { databases :: Array String
    , pursPkg :: String
    , scalaPkg :: String
    }

getSchemaConfig :: OptParse.Parser (Either String SchemaConfig)
getSchemaConfig = ado
  domains <- domainsOption
  database <- databaseOption
  maybeTable <- tableOption
  scalaPkg <- scalaPkgOption
  pursPkg <- pursPkgOption
  dbManagedColumns <- dbManagedColumnsOption
  in case domains, maybeTable of
    true, Nothing ->
      Right
        $ Domains
            { databases: String.split (Pattern "::") database
            , pursPkg
            , scalaPkg
            }
    false, (Just "") -> Left "The table option requires a value."
    false, (Just table) ->
      Right
        $ Table
            { database
            , dbManagedColumns
            , pursPkg
            , scalaPkg
            , table
            }
    _, _ -> Left "The table option should be used iff the domain option is not."

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

dbManagedColumnsOption :: OptParse.Parser (Maybe (Array String))
dbManagedColumnsOption =
  let
    split = String.split (Pattern ",")

    arrayStringParse = map split OptParse.str
  in
    OptParse.Types.optional
      $ OptParse.option arrayStringParse
      $ fold
          [ OptParse.long "db-managed-columns"
          , OptParse.metavar "DbManaged Columns"
          , OptParse.help "Columns to set the dbManaged annotation"
          ]
