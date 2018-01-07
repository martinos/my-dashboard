module ViewHelper exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import RemoteData exposing (RemoteData(Success, NotAsked, Loading, Failure))


css : String -> Html msg
css path =
    node "link" [ rel "stylesheet", href path ] []


foundation =
    css "https://cdnjs.cloudflare.com/ajax/libs/foundation/6.4.3/css/foundation.min.css"


twitterBootstrap =
    css "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css"


bulma =
    css "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.6.1/css/bulma.min.css"


debug : Attribute msg
debug =
    style [ ( "background-color", "red" ) ]


onChange tagger =
    on "change" (JD.map tagger targetValue)


viewWebData : RemoteData.WebData (Html msg) -> Html msg
viewWebData viewData =
    case viewData of
        Success data ->
            data

        NotAsked ->
            div [ class "callout secondary" ] [ text "Not Asked" ]

        Loading ->
            div [ class "callout secondary" ] [ text "Loading..." ]

        Failure e ->
            div [ class "callout alert" ] [ e |> toString |> text ]
