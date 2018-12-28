module Main exposing (Model(..), Msg(..), getRandomCatGif, gifsDecoder, init, main, subscriptions, update, view, viewGif, viewKeyedEntry)

import Browser
import Debounce exposing (Debounce)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Http
import Json.Decode exposing (Decoder, field, list, map3, string)
import Task exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Config =
    { gifs : List Gif
    , visibleGifs : List Gif
    , searchTerm : String
    , debounce : Debounce String
    , selectedGif : Maybe Gif
    }


type alias Gif =
    { id : String
    , title : String
    , embed_url : String
    }


type Model
    = Failure
    | Loading
    | Success Config


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRandomCatGif )



-- UPDATE


type Msg
    = MorePlease
    | GotGifs (Result Http.Error (List Gif))
    | OnInput String
    | DebounceMsg Debounce.Msg
    | Filter String
    | SelectedGif Gif


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getRandomCatGif )

        OnInput s ->
            case model of
                Success config ->
                    let
                        -- Push your values here.
                        ( debounce, cmd ) =
                            Debounce.push debounceConfig s config.debounce

                        -- Can I already reference the debounce in the "let" branch? TODO
                        newConfig =
                            { config | searchTerm = s, debounce = debounce }
                    in
                    ( Success newConfig, cmd )

                _ ->
                    ( Loading, getRandomCatGif )

        DebounceMsg msg_ ->
            case model of
                Success config ->
                    let
                        ( debounce, cmd ) =
                            Debounce.update
                                debounceConfig
                                (Debounce.takeLast save)
                                msg_
                                config.debounce

                        newConfig =
                            { config | debounce = debounce }
                    in
                    ( Success newConfig, cmd )

                _ ->
                    ( Loading, getRandomCatGif )        

        Filter searchTerm ->
            case model of
                Success config ->
                    let
                        newConfig =
                            { config
                                | searchTerm = searchTerm
                                , visibleGifs = getVisibleGifs config.gifs searchTerm
                            }
                    in
                    ( Success newConfig, Cmd.none )

                _ ->
                    ( Loading, getRandomCatGif )

        SelectedGif gif ->
            case model of
                Success config ->
                    let
                        newConfig =
                            { config | selectedGif = Just gif }
                    in
                    ( Success newConfig, Cmd.none )

                _ ->
                    ( Loading, getRandomCatGif )

        GotGifs result ->
            case result of
                Ok gifs ->
                    let
                        config =
                            Config gifs gifs "" Debounce.init Nothing
                    in
                    ( Success config, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = DebounceMsg
    }


save : String -> Cmd Msg
save s =
    Task.perform Filter (Task.succeed s)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Trending Gifs" ]
        , div [] [ viewGif model ]
        , div [] [ viewSelectedGif model ]
        ]


viewSelectedGif : Model -> Html Msg
viewSelectedGif model =
    case model of
        Success config ->
            case config.selectedGif of
                Nothing ->
                    div [] [ text "nothing selected" ]

                Just gif ->
                    div []
                        [ h3 [] [ text (gif.id ++ " selected") ]
                        , img [ src gif.embed_url ] []
                        ]

        _ ->
            div [] [ text "please select a gif" ]


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

        Success config ->
            div []
                [ div []
                    [ viewInput "text" "Search..." OnInput
                    ]
                , button [ onClick MorePlease, style "display" "block" ] [ text "more please" ]
                , div [] [ text (String.concat [ String.fromInt (List.length config.visibleGifs), " gifs found" ]) ]
                , div [] [ text (String.concat [ "Searching for : ", config.searchTerm ]) ]
                , Keyed.ul [ class "gif-entry" ] <|
                    List.map viewKeyedEntry config.visibleGifs
                ]


viewInput : String -> String -> (String -> msg) -> Html msg
viewInput t p toMsg =
    input [ type_ t, placeholder p, onInput toMsg ] []


viewKeyedEntry : Gif -> ( String, Html Msg )
viewKeyedEntry gif =
    ( gif.id
    , li []
        [ a [ href "#", onClick (SelectedGif gif) ] [ text gif.title ]
        ]
    )


getVisibleGifs : List Gif -> String -> List Gif
getVisibleGifs allGifs searchTerm =
    List.filter (\gif -> String.contains searchTerm gif.title) allGifs



-- HTTP


getRandomCatGif : Cmd Msg
getRandomCatGif =
    Http.get
        { url = "https://api.giphy.com/v1/gifs/trending?api_key=dc6zaTOxFJmzC&tag=cat"
        , expect = Http.expectJson GotGifs gifsDecoder
        }


gifsDecoder : Decoder (List Gif)
gifsDecoder =
    field "data"
        (list
            (Json.Decode.map3 Gif
                (field "id" string)
                (field "title" string)
                (field "embed_url" string)
            )
        )
