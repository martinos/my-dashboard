module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import ViewHelper exposing (css, viewWebData)
import RemoteData as RD exposing (RemoteData(..), WebData)
import HttpHelpers exposing (..)
import Json.Decode as JD
import Regex exposing (..)
import ElmEscapeHtml exposing (unescape)


bulma =
    div []
        [ node "meta" [ attribute "name" "viewport", attribute "content" "width=device-width, initial-scale=1" ] []
        , css "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.6.1/css/bulma.min.css"
        ]


type alias Question =
    { title : String, link : String, tags : List String }


type alias Repo =
    { name : String, language : Maybe String, fork : Bool, url : String }


type alias Model =
    { repoInput : String
    , repos : WebData (List Repo)
    , starsInput : String
    , stars : WebData (List Repo)
    , soStarsInput : String
    , soStars : WebData (List Question)
    }


model : Model
model =
    { repoInput = "", repos = NotAsked, starsInput = "", stars = NotAsked, soStarsInput = "", soStars = NotAsked }


repos : List Repo
repos =
    [ { name = "Phoenix", language = Just "Elixir", fork = True, url = "http://coucou.com" } ]


main =
    Html.program
        { init =
            ( model
            , Cmd.batch
                [ getRepos
                , getStars
                , getSOStars 89082
                ]
            )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type Msg
    = SetRepos (WebData (List Repo))
    | SetStars (WebData (List Repo))
    | SetSOStars (WebData (List Question))
    | SetReposFilter String
    | SetStarsFilter String
    | SetSOStarsFilter String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRepos repos ->
            ( { model | repos = repos }, Cmd.none )

        SetStars repos ->
            ( { model | stars = repos }, Cmd.none )

        SetSOStars stars ->
            ( { model | soStars = stars }, Cmd.none )

        SetReposFilter str ->
            ( { model | repoInput = str }, Cmd.none )

        SetStarsFilter str ->
            ( { model | starsInput = str }, Cmd.none )

        SetSOStarsFilter str ->
            ( { model | soStarsInput = str }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ bulma
        , section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-two-thirds" ]
                        [ div [ class "columns" ]
                            [ div [ class "column" ] [ h1 [ class "title" ] [ text "Github" ] ] ]
                        , div [ class "columns" ]
                            [ reposColumns model
                            , starsRepoColumns model
                            ]
                        ]
                    , div [ class "column" ]
                        [ div [ class "columns" ]
                            [ div [ class "column" ] [ h1 [ class "title" ] [ text "StackOverflow" ] ] ]
                        , div [ class "columns" ] [ stackOverFlowColumns model ]
                        ]
                    ]
                ]
            ]
        ]


reposColumns model =
    div [ class "column" ]
        [ h1 [ class "subtitle" ] [ text "Repos" ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Filter" ]
            , div [ class "control" ]
                [ input [ type_ "text", class "input", onInput SetReposFilter, attribute "autofocus" "" ] [] ]
            ]
        , model.repos |> RD.map (List.filter (filterRepo (model.repoInput))) |> RD.map viewRepos |> viewWebData
        ]


starsRepoColumns model =
    div [ class "column" ]
        [ h1 [ class "subtitle" ] [ text "Stars" ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Filter" ]
            , div [ class "control" ]
                [ input [ type_ "text", class "input", onInput SetStarsFilter ] [] ]
            ]
        , model.stars |> RD.map (List.filter (filterRepo (model.starsInput))) |> RD.map viewRepos |> viewWebData
        ]


stackOverFlowColumns model =
    div [ class "column" ]
        [ h1 [ class "subtitle" ] [ text "Stars" ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Filter" ]
            , div [ class "control" ]
                [ input [ type_ "text", class "input", onInput SetSOStarsFilter ] [] ]
            ]
        , model.soStars |> RD.map (List.filter (filterQuestion model.soStarsInput)) |> RD.map questionTable |> viewWebData
        ]


questionTable : List Question -> Html msg
questionTable questions =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Title" ]
                , th [] [ text "Tags" ]
                ]
            ]
        , tbody [] (questions |> List.map viewQuestion)
        ]


viewQuestion : Question -> Html msg
viewQuestion question =
    tr []
        [ td [] [ a [ href question.link ] [ question.title |> text ] ]
        , td [] [ question.tags |> String.join ", " |> text ]
        ]


filterRepo str repo =
    (repo.name |> filter str) || (repo.language |> Maybe.withDefault "" |> filter str)


filterQuestion str question =
    (question.title |> filter str) || (question.tags |> List.filter (filter str) |> List.isEmpty |> not)


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
        , tbody [] (repos |> List.map viewRepo)
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
        >> addQuery ( "per_page", "100" )
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
        >> addQuery ( "per_page", "100" )
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


soServer =
    withScheme "https"
        >> withHost "api.stackexchange.com"
        >> toHttpRequest
        >> RD.sendRequest


getSOStars userID =
    get
        |> withPath ("/2.2/users/" ++ (userID |> toString) ++ "/favorites")
        >> addQueries [ ( "order", "desc" ), ( "sort", "activity" ), ( "site", "stackoverflow" ), ( "pagesize", "100" ) ]
        >> withJsonResp questionsDecoder
        >> soServer
        >> Cmd.map SetSOStars


questionsDecoder =
    JD.field "items" (JD.list decodeItem)


decodeItem =
    JD.map3 Question
        (JD.map unescape (JD.field "title" JD.string))
        (JD.field "link" JD.string)
        (JD.field "tags" (JD.list JD.string))
