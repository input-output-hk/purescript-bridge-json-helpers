{ name = "purescript-bridge-json-helpers"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
