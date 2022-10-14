let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220725/packages.dhall
        sha256:e56fbdf33a5afd2a610c81f8b940b413a638931edb41532164e641bb2a9ec29c

let overrides = {=}

let additions =
      { boxes =
        { dependencies = [ "profunctor", "prelude", "stringutils", "strings" ]
        , repo = "https://github.com/cdepillabout/purescript-boxes.git"
        , version = "6ecd6d6e52ec6cc13062387b69dddf3aae9ea70f"
        }
      , bytestrings =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "exceptions"
          , "foldable-traversable"
          , "integers"
          , "leibniz"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "quickcheck-laws"
          , "quotient"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/martyall/purescript-bytestrings.git"
        , version = "e51cf868a4137c1c48c98d32115bb2014c9f7624"
        }
      , foreign-generic =
        { dependencies =
          [ "arrays"
          , "assert"
          , "bifunctors"
          , "console"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "identity"
          , "lists"
          , "maybe"
          , "newtype"
          , "partial"
          , "prelude"
          , "record"
          , "strings"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/jsparkes/purescript-foreign-generic.git"
        , version = "844f2ababa2c7a0482bf871e1e6bf970b7e51313"
        }
      , postgresql-client =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "arrays"
          , "assert"
          , "bifunctors"
          , "bytestrings"
          , "datetime"
          , "decimals"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-generic"
          , "foreign-object"
          , "identity"
          , "integers"
          , "js-date"
          , "lists"
          , "maybe"
          , "newtype"
          , "node-process"
          , "nullable"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "psci-support"
          , "string-parsers"
          , "strings"
          , "test-unit"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          , "validation"
          ]
        , repo = "https://github.com/dstevenson1/purescript-postgresql-client.git"
        , version = "1448658d2ffebb407a0badc96be8ef9f0baaa938"
        }
      }

in  upstream // overrides // additions
