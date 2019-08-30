module Main exposing (main)

import Api
import Browser
import Element exposing (..)
import Element.Background as Background
import Http
import RemoteData exposing (RemoteData)


type alias ErrorWithMetadata =
    ( Http.Error, Maybe { metadata : Http.Metadata, body : String } )


type alias WebData a =
    RemoteData ErrorWithMetadata a


type alias WebResult a =
    Result ErrorWithMetadata a



---- MODEL ----


type alias Model =
    { roundtripIntProgress : WebData Int
    }


init : ( Model, Cmd Msg )
init =
    ( { roundtripIntProgress = RemoteData.Loading }
    , Cmd.batch
        [ Cmd.map GotRoundtripIntResponse <| Api.postRoundtripInt 41
        ]
    )



---- UPDATE ----


type Msg
    = GotRoundtripIntResponse (WebResult Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRoundtripIntResponse result ->
            ( { model | roundtripIntProgress = RemoteData.fromResult result }, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ width fill, padding 30 ]
        [ el
            [ paddingXY 20 10
            , Background.color <| remoteDataColor model.roundtripIntProgress 41
            ]
          <|
            text "postRoundtripInt"
        ]


remoteDataColor : RemoteData e a -> a -> Color
remoteDataColor data expected =
    case data of
        RemoteData.NotAsked ->
            rgb 1 1 1

        RemoteData.Loading ->
            rgb 1 1 0.5

        RemoteData.Success actual ->
            if expected == actual then
                rgb 0.5 1 0.5

            else
                rgb 1 0.5 0.5

        RemoteData.Failure _ ->
            rgb 1 0.5 0.5



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = layout [] << view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
