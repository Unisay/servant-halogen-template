{ name =
    "servant-halogen-frontend"
, dependencies =
    [ "aff"
    , "aff-bus"
    , "affjax"
    , "argonaut"
    , "argonaut-codecs"
    , "argonaut-core"
    , "console"
    , "css"
    , "effect"
    , "formatters"
    , "fusionauth"
    , "halogen"
    , "halogen-bulma"
    , "halogen-css"
    , "halogen-formless"
    , "nonempty"
    , "now"
    , "precise-datetime"
    , "prelude"
    , "remotedata"
    , "routing"
    , "routing-duplex"
    , "slug"
    , "typelevel-prelude"
    , "variant"
    , "unicode"
    , "uuid"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
