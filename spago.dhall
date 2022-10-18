{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ccap-codegen"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "boxes"
  , "console"
  , "control"
  , "decimals"
  , "effect"
  , "either"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "foreign-object"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "node-process"
  , "nonempty"
  , "optparse"
  , "ordered-collections"
  , "parsing"
  , "postgresql-client"
  , "prelude"
  , "spec"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "unicode"
  , "unsafe-coerce"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
