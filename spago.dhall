{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "boxes"
    , "console"
    , "debug"
    , "effect"
    , "generics-rep"
    , "node-fs"
    , "node-process"
    , "parsing"
    , "postgresql-client"
    , "prelude"
    , "psci-support"
    , "record"
    , "strings"
    , "transformers"
    , "yargs"
    ]
, packages =
    ./packages.dhall
}
