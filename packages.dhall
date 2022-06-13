let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8/packages.dhall sha256:0e95ec11604dc8afc1b129c4d405dcc17290ce56d7d0665a0ff15617e32bbf03

let overrides = {=}

let additions =
      { boxes =
        { dependencies =
          [ "profunctor", "prelude", "stringutils", "generics-rep", "strings" ]
        , repo = "https://github.com/cdepillabout/purescript-boxes.git"
        , version = "v2.0.1"
        }
      , postgresql-client =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "arrays"
          , "assert"
          , "bifunctors"
          , "bytestrings"
          , "console"
          , "datetime"
          , "decimals"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-generic"
          , "foreign-object"
          , "js-date"
          , "lists"
          , "maybe"
          , "newtype"
          , "nullable"
          , "prelude"
          , "psci-support"
          , "test-unit"
          , "transformers"
          , "tuples"
          ]
        , repo = "https://github.com/pbrant/purescript-postgresql-client.git"
        , version = "7188cd732eb7981910915fb5e8ab8b158abda95b"
        }
      }

in  upstream // overrides // additions
