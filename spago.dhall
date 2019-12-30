{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ccap-codegen"
, dependencies =
    [ "argonaut-codecs"
    , "console"
    , "boxes"
    , "debug"
    , "effect"
    , "filterable"
    , "generics-rep"
    , "node-fs"
    , "node-process"
    , "parsing"
    , "postgresql-client"
    , "prelude"
    , "psci-support"
    , "record"
    , "spec"
    , "strings"
    , "transformers"
    , "typelevel-prelude"
    , "yargs"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
