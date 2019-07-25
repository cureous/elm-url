module Main exposing (Model, Msg(..), Route(..), init, locationToUrl, main, route, routeToString, subscriptions, update, view, viewLink, viewRoute)

import Html exposing (Html, a, button, code, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Navigation exposing (Location)
import Url as Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), (<?>), int, s, top)
import Url.Parser.Query as Query



-- type alias Url =
--     { protocol : Protocol
--     , host : String
--     , port_ : Maybe Int
--     , path : String
--     , query : Maybe String
--     , fragment : Maybe String
--     }
--    location: { href = "http://localhost:5000/",
--    host = "localhost:5000",
--    hostname = "localhost",
--    protocol = "http:",
--    origin = "http://localhost:5000",
--    port_ = "5000",
--    pathname = "/",
--    search = "",
--    hash = "#123",
--    username = <internal structure>,
--    password = <internal structure> }
-- type alias Location =
--     { href : String
--     , host : String
--     , hostname : String
--     , protocol : String
--     , origin : String
--     , port_ : String
--     , pathname : String
--     , search : String
--     , hash : String
--     , username : String
--     , password : String
--     }


locationToUrl : Location -> Url
locationToUrl l =
    let
        protocol =
            case l.protocol of
                "https:" ->
                    Url.Https

                _ ->
                    Url.Http

        search =
            String.dropLeft 1 l.search
    in
    { protocol = protocol
    , host = l.host
    , port_ = String.toInt l.port_ |> Result.toMaybe
    , path = l.pathname
    , fragment =
        if String.isEmpty l.hash then
            Nothing

        else
            Just l.hash
    , query =
        if String.isEmpty search then
            Nothing

        else
            Just search
    }



-- urlToLocation : Url -> Location


main =
    Navigation.program (locationToUrl >> Debug.log "url change" >> UrlChange)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { history : List (Maybe Route)
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        _ =
            Debug.log "location" location

        _ =
            Debug.log "tourl" <| locationToUrl location
    in
    ( Model [ UrlParser.parse route (locationToUrl location) ]
    , Cmd.none
    )



-- URL PARSING


type Route
    = Home
    | BlogList (Maybe String)
    | BlogPost Int


route : UrlParser.Parser (Route -> a) a
route =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map BlogList (s "blog" <?> Query.string "search")
        , UrlParser.map BlogPost (s "blog" </> int)
        ]



--
-- UPDATE


type Msg
    = NewUrl String
    | UrlChange Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            ( model
            , Navigation.newUrl url
            )

        -- --
        UrlChange location ->
            ( { model | history = UrlParser.parse route location :: model.history }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Links" ]
        , ul [] (List.map viewLink [ "/", "/blog/", "/blog/42", "/blog/37", "/blog/?search=cats" ])
        , h1 [] [ text "History" ]
        , ul [] (List.map viewRoute model.history)
        ]


viewLink : String -> Html Msg
viewLink url =
    li [] [ button [ onClick (NewUrl url) ] [ text url ] ]


viewRoute : Maybe Route -> Html msg
viewRoute maybeRoute =
    case maybeRoute of
        Nothing ->
            li [] [ text "Invalid URL" ]

        Just route ->
            li [] [ code [] [ text (routeToString route) ] ]


routeToString : Route -> String
routeToString route =
    case route of
        Home ->
            "home"

        BlogList Nothing ->
            "list all blog posts"

        BlogList (Just search) ->
            "search for " ++ Http.encodeUri search

        BlogPost id ->
            "show blog " ++ toString id
