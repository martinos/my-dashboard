module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import ViewHelper exposing (css, viewWebData)
import RemoteData as RD exposing (RemoteData(..), WebData)
import HttpHelpers exposing (..)
import Json.Decode as JD
import Regex exposing (..)


bulma =
    css "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.6.1/css/bulma.min.css"


type alias Repo =
    { name : String, language : Maybe String, fork : Bool, url : String }


type alias Model =
    { repoInput : String, repos : WebData (List Repo), starsInput : String, stars : WebData (List Repo) }


model : Model
model =
    { repoInput = "", repos = NotAsked, starsInput = "", stars = NotAsked }


repos : List Repo
repos =
    [ { name = "Phoenix", language = Just "Elixir", fork = True, url = "http://coucou.com" } ]


main =
    Html.program { init = ( model, Cmd.batch [ getRepos, getStars ] ), update = update, view = view, subscriptions = always Sub.none }


type Msg
    = SetRepos (WebData (List Repo))
    | SetStars (WebData (List Repo))
    | SetReposFilter String
    | SetStarsFilter String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRepos repos ->
            ( { model | repos = repos }, Cmd.none )

        SetStars repos ->
            ( { model | stars = repos }, Cmd.none )

        SetReposFilter str ->
            ( { model | repoInput = str }, Cmd.none )

        SetStarsFilter str ->
            ( { model | starsInput = str }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ bulma
        , section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ h1 [ class "title" ] [ text "Github" ] ]
                , div [ class "columns" ]
                    [ div [ class "column" ]
                        [ h1 [ class "subtitle" ] [ text "Repos" ]
                        , div [ class "field" ]
                            [ label [ class "label" ] [ text "Filter" ]
                            , div [ class "control" ]
                                [ input [ type_ "text", class "input", onInput SetReposFilter, attribute "autofocus" "" ] [] ]
                            ]
                        , model.repos |> RD.map (List.filter (filterRepo (model.repoInput))) |> RD.map viewRepos |> viewWebData
                        ]
                    , div [ class "column" ]
                        [ h1 [ class "subtitle" ] [ text "Stars" ]
                        , div [ class "field" ]
                            [ label [ class "label" ] [ text "Filter" ]
                            , div [ class "control" ]
                                [ input [ type_ "text", class "input", onInput SetStarsFilter ] [] ]
                            ]
                        , model.stars |> RD.map (List.filter (.name >> filter (model.starsInput))) |> RD.map viewRepos |> viewWebData
                        ]
                    ]
                ]
            ]
        ]


filterRepo str repo =
    (repo.name |> filter str) || (repo.name |> filter str)


viewRepos : List Repo -> Html msg
viewRepos repos =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ td [] [ text "Repo" ]
                , td [] [ text "Language" ]
                , td [] [ text "Fork" ]
                ]
            ]
        , tbody []
            (repos |> List.map viewRepo)
        ]


viewRepo : Repo -> Html msg
viewRepo repo =
    tr []
        [ td [] [ a [ href repo.url ] [ text repo.name ] ]
        , td [] [ repo.language |> Maybe.withDefault "N/A" |> text ]
        , td [] [ repo.fork |> toString |> text ]
        ]


getRepos : Cmd Msg
getRepos =
    get
        |> reposPath
        >> withJsonResp (JD.list repoDecoder)
        >> server
        >> Cmd.map SetRepos


repoDecoder =
    JD.map4 Repo
        (JD.field "name" JD.string)
        (JD.field "language" (JD.maybe JD.string))
        (JD.field "fork" JD.bool)
        (JD.field "html_url" JD.string)


reposPath =
    withPath "/users/martinos/repos"


getStars =
    get
        |> starsPath
        >> withJsonResp (JD.list repoDecoder)
        >> server
        >> Cmd.map SetStars


starsPath =
    withPath "/users/martinos/starred"


server =
    withScheme "https"
        >> withHost "api.github.com"
        >> toHttpRequest
        >> RD.sendRequest


filter : String -> String -> Bool
filter =
    regex >> caseInsensitive >> contains
