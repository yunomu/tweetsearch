module Route exposing (Route(..), fromUrl, path)

import Url exposing (Url)
import Url.Builder as UrlBuilder
import Url.Parser as P exposing ((</>), (<?>), Parser, s)
import Url.Parser.Query as Query


type Route
    = Index
    | User String
    | NotFound


parser : Parser (Route -> a) a
parser =
    P.oneOf
        [ P.map Index P.top
        , P.map User P.string
        ]


path : Route -> String
path route =
    case route of
        Index ->
            UrlBuilder.absolute [] []

        _ ->
            UrlBuilder.absolute [] []


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (P.parse parser url)
