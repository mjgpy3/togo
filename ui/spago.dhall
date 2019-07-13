{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "my-project"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "argonaut-codecs"
    , "console"
    , "effect"
    , "halogen"
    , "halogen-css"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
}
