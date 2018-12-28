module Main exposing (Model(..), Msg(..), getRandomCatGif, gifsDecoder, init, main, subscriptions, update, view, viewGif, viewKeyedEntry)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Http
import Json.Decode exposing (Decoder, field, list, string)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success (List String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRandomCatGif )



-- UPDATE


type Msg
    = MorePlease
    | GotGifs (Result Http.Error (List String))
    | Filter String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getRandomCatGif )

        Filter searchTerm ->
            ( Loading, getRandomCatGif )

        GotGifs result ->
            case result of
                Ok urls ->
                    ( Success urls, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Trending Gifs" ]
        , viewGif model
        ]


viewGif : Model -> Html Msg
viewGif model =
    case model of
        Failure ->
            div []
                [ text "I could not load a random cat for some reason. "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success urls ->
            div []
                [ div []
                    [ viewInput "text" "Search..." Filter
                    ]
                , button [ onClick MorePlease, style "display" "block" ] [ text "more please" ]
                , div [] [ text (String.concat [ String.fromInt (List.length urls), " gifs found" ]) ]
                , Keyed.ul [ class "gif-entry" ] <|
                    List.map viewKeyedEntry urls
                ]


viewInput : String -> String -> (String -> msg) -> Html msg
viewInput t p toMsg =
    input [ type_ t, placeholder p, onInput toMsg ] []


viewKeyedEntry : String -> ( String, Html Msg )
viewKeyedEntry gif =
    ( gif
    , li []
        [ text gif
        ]
    )



-- HTTP


getRandomCatGif : Cmd Msg
getRandomCatGif =
    Http.get
        { url = "https://api.giphy.com/v1/gifs/trending?api_key=dc6zaTOxFJmzC&tag=cat"
        , expect = Http.expectJson GotGifs gifsDecoder
        }


gifsDecoder : Decoder (List String)
gifsDecoder =
    field "data" (list (field "title" string))
