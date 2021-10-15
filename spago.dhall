{ name = "purescript-bridge-json-helpers"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "console"
  , "contravariant"
  , "control"
  , "effect"
  , "either"
  , "foreign-object"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "record"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
