module Main exposing (Model(..), Msg(..), getGifsByTag, gifsDecoder, init, main, subscriptions, update, view, viewGif, viewKeyedEntry)

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
    , tag : String
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
    ( Loading, getGifsByTag "cat" )



-- UPDATE


type Msg
    = LoadCats
    | MorePlease String
    | GotGifs (Result Http.Error (List Gif))
    | OnInput String
    | DebounceMsg Debounce.Msg
    | Filter String
    | SelectedGif Gif


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadCats ->
            ( Loading, getGifsByTag "cat" )

        MorePlease tag ->
            ( Loading, getGifsByTag tag )

        OnInput s ->
            case model of
                Success config ->
                    let
                        -- Push your values here.
                        ( debounce, cmd ) =
                            Debounce.push debounceConfig s config.debounce

                        newConfig =
                            { config | tag = s, debounce = debounce }
                    in
                    ( Success newConfig, cmd )

                _ ->
                    ( Loading, getGifsByTag "cat" )

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
                    ( Loading, getGifsByTag "cat" )

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
                    ( Loading, getGifsByTag "cat" )

        SelectedGif gif ->
            case model of
                Success config ->
                    let
                        newConfig =
                            { config | selectedGif = Just gif }
                    in
                    ( Success newConfig, Cmd.none )

                _ ->
                    ( Loading, getGifsByTag "cat" )

        GotGifs result ->
            case result of
                Ok gifs ->
                    let
                        config =
                            Config gifs gifs "" "" Debounce.init Nothing
                    in
                    ( Success config, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 600
    , transform = DebounceMsg
    }


save : String -> Cmd Msg
save s =
    Task.perform MorePlease (Task.succeed s)



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
                [ text "I could not load the gifs for some reason. "
                , button [ onClick LoadCats ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success config ->
            div []
                [ div []
                    [ viewInput "text" "Fetch by tag..." OnInput
                    , viewInput "text" "Filter..." Filter
                    ]

                -- , button [ onClick MorePlease, style "display" "block" ] [ text "more please" ]
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


getGifsByTag : String -> Cmd Msg
getGifsByTag tag =
    Http.get
        { url = "https://api.giphy.com/v1/gifs/search?api_key=dc6zaTOxFJmzC&q=" ++ tag
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
