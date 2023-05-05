let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall
        sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221201/packages.dhall
        sha256:d1a68fa15709eaa686515eb5b9950d82c743f7bf73e3d87a4abe9e1be6fda571

let overrides = {=}

{- Additional dependencies to support the new version of postgresql-client -}
let additional_dependencies =
      upstream
      with polyform =
          mkPackage
            [ "heterogeneous"
            , "js-unsafe-stringify"
            , "newtype"
            , "ordered-collections"
            , "variant"
            , "profunctor"
            , "invariant"
            , "foreign-object"
            , "run"
            , "transformers"
            , "validation"
            , "foreign"
            ]
            "https://github.com/purescript-polyform/polyform.git"
            "v0.9.2"
      with polyform-batteries-core =
          mkPackage
            [ "debug"
            , "decimals"
            , "filterable"
            , "numbers"
            , "polyform"
            , "prelude"
            , "record-extra"
            , "test-unit"
            ]
            "https://github.com/purescript-polyform/batteries-core.git"
            "v0.3.0"
      with polyform-batteries-urlencoded =
          mkPackage
            [ "argonaut"
            , "console"
            , "debug"
            , "effect"
            , "form-urlencoded"
            , "polyform-batteries-core"
            , "psci-support"
            , "spec"
            ]
            "https://github.com/purescript-polyform/batteries-urlencoded.git"
            "v0.4.1"
      with polyform-batteries-env =
        { dependencies =
          [ "arrays"
          , "identity"
          , "maybe"
          , "ordered-collections"
          , "polyform"
          , "polyform-batteries-core"
          , "prelude"
          , "psci-support"
          , "typelevel-prelude"
          ]
        , repo = "https://github.com/purescript-polyform/batteries-env.git"
        , version = "v0.2.0"
        }
      with js-unsafe-stringify =
          mkPackage
            ([] : List Text)
            "https://github.com/paluh/purescript-js-unsafe-stringify.git"
            "master"
      with quotient =
          mkPackage
            [ "prelude", "quickcheck" ]
            "https://github.com/rightfold/purescript-quotient.git"
            "v3.0.0"

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
          , "debug"
          , "decimals"
          , "dotenv"
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
          , "js-unsafe-stringify"
          , "lists"
          , "maybe"
          , "newtype"
          , "node-process"
          , "nullable"
          , "numbers"
          , "ordered-collections"
          , "partial"
          , "polyform"
          , "polyform-batteries-core"
          , "polyform-batteries-env"
          , "prelude"
          , "profunctor"
          , "psci-support"
          , "record"
          , "string-parsers"
          , "strings"
          , "test-unit"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          , "validation"
          ]
        , repo = "https://github.com/rightfold/purescript-postgresql-client.git"
        , version = "b2c8f7aa5c09c88afb62195f6f5654b01d11588f"
        }
      }

in  upstream // overrides // additions // additional_dependencies
