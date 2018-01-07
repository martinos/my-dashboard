module HttpHelpers exposing (..)

import Html exposing (..)
import Http exposing (Header)
import Time exposing (Time)
import Json.Encode as JE
import Json.Decode as JD
import BasicAuth exposing (buildAuthorizationHeader)


productionServer : Request a -> Request a
productionServer =
    withScheme "http"
        >> withHost "localhost"
        >> withPort "3000"


chatMessagesPath : Request a -> Request a
chatMessagesPath =
    withPath "admin/chat_messages"


authenticated : Request a -> Request a
authenticated =
    addHeaders [ buildAuthorizationHeader "martin" "chabot" ]



-- server =
--     authenticated
--         >> productionServer
--         |> run_


apply : (Request JD.Value -> Request a) -> Http.Request a
apply builder =
    builder emptyRequest |> toHttpRequestData |> Debug.log "Request" |> Http.request


toHttpRequest : Request a -> Http.Request a
toHttpRequest =
    toHttpRequestData >> Debug.log "Request" >> Http.request


{-| In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
says a URL looks like this:
```
  https://example.com:8042/over/there?name=ferret#nose
  \___/   \______________/\_________/ \_________/ \__/
    |            |            |            |        |
  scheme     authority       path        query   fragment
```
-}
type alias Url =
    { scheme : String
    , host : String
    , port_ : String
    , path : String
    , query : String
    , fragment : String
    }


withScheme : String -> Request a -> Request a
withScheme scheme request =
    { request | url = request.url |> updateScheme scheme }


withHost : String -> Request a -> Request a
withHost host request =
    { request | url = request.url |> updateHost host }


withPort : String -> Request a -> Request a
withPort port_ request =
    { request | url = request.url |> updatePort port_ }


withPath : String -> Request a -> Request a
withPath path request =
    { request | url = request.url |> updatePath path }


withQueryString : String -> Request a -> Request a
withQueryString query request =
    { request | url = request.url |> updateQuery query }


withFragment : String -> Request a -> Request a
withFragment fragment request =
    { request | url = request.url |> updateFragment fragment }


updateScheme : String -> Url -> Url
updateScheme scheme url =
    { url | scheme = scheme }


updateHost : String -> Url -> Url
updateHost host url =
    { url | host = host }


updatePort : String -> Url -> Url
updatePort port_ url =
    { url | port_ = port_ }


updatePath : String -> Url -> Url
updatePath path url =
    { url | path = path }


updateQuery : String -> Url -> Url
updateQuery query url =
    { url | query = query }


updateFragment : String -> Url -> Url
updateFragment fragment url =
    { url | fragment = fragment }


defaultUrl : Url
defaultUrl =
    { scheme = "http", host = "localhost", port_ = "", path = "", query = "", fragment = "" }


urlToString : Url -> String
urlToString url =
    (url.scheme
        ++ "://"
        ++ url.host
        |> appendPort url.port_
    )
        ++ url.path
        |> appendQuery url.query
        |> appendFragment url.fragment
        |> Debug.log "Request"


toHttpRequestData : Request a -> HttpRequestData a
toHttpRequestData request =
    { request | url = request.url |> urlToString }


appendPort port_ url =
    if String.isEmpty port_ then
        url
    else
        url ++ ":" ++ port_


appendQuery query url =
    if String.isEmpty query then
        url
    else
        url ++ "?" ++ query


appendFragment fragment url =
    if String.isEmpty fragment then
        url
    else
        url ++ "#" ++ fragment


type alias Request a =
    { method : String
    , headers : List Header
    , url : Url
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Time
    , withCredentials : Bool
    }



-- This is the format for the request creation


type alias HttpRequestData a =
    { method : String
    , headers : List Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Time
    , withCredentials : Bool
    }


emptyRequest =
    { method = "GET"
    , headers = []
    , url = defaultUrl
    , body = Http.emptyBody
    , expect = Http.expectJson JD.value
    , timeout = Nothing
    , withCredentials = False
    }


withMethod : String -> Request a -> Request a
withMethod method request =
    { request | method = method }


get : Request JD.Value
get =
    emptyRequest |> withMethod "GET"


post : Request JD.Value
post =
    emptyRequest |> withMethod "POST"


put : Request JD.Value
put =
    emptyRequest |> withMethod "PUT"


patch : Request JD.Value
patch =
    emptyRequest |> withMethod "PATCH"


delete : Request JD.Value
delete =
    emptyRequest |> withMethod "DELETE"


withHeaders : List Header -> Request a -> Request a
withHeaders headers request =
    { request | headers = headers }


addHeaders : List Header -> Request a -> Request a
addHeaders headers request =
    { request | headers = headers ++ request.headers }


withJsonBody : JE.Value -> Request a -> Request a
withJsonBody value request =
    { request | body = Http.jsonBody value }


withExpect : Http.Expect a -> Request a -> Request a
withExpect expect request =
    { request | expect = expect }


withJsonResp : JD.Decoder b -> Request a -> Request b
withJsonResp decoder request =
    { request | expect = Http.expectJson decoder }
        |> addHeaders [ Http.header "Accept" "application/json" ]


addQueries : List ( String, String ) -> Request a -> Request a
addQueries queryParams request =
    queryParams |> List.foldr (\query request -> request |> addQuery query) request


addQuery : ( String, String ) -> Request a -> Request a
addQuery queryParams request =
    request
        |> withQueryString
            (request.url.query ++ "&" ++ queryPair queryParams)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    queryEscape key ++ "=" ++ queryEscape value


replace : String -> String -> String -> String
replace old new =
    String.split old >> String.join new


queryEscape : String -> String
queryEscape =
    Http.encodeUri >> replace "%20" "+"
