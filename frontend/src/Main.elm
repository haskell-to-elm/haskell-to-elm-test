module Main exposing (main)

import Api
import Browser
import Element exposing (..)
import Element.Background as Background
import Http
import RemoteData exposing (RemoteData)
import Roundtrip
import WebData exposing (..)



---- MODEL ----


type alias Model =
    { roundtripInt : Roundtrip.Model Int
    , roundtripTuple : Roundtrip.Model ( Int, String )
    }


init : ( Model, Cmd Msg )
init =
    let
        ( roundtripInt, roundtripIntCmd ) =
            Roundtrip.init
                { name = "roundtripInt"
                , arbitrary = Api.getArbitraryInts
                , roundtrip = Api.postRoundtripInt
                }

        ( roundtripTuple, roundtripTupleCmd ) =
            Roundtrip.init
                { name = "roundtripTuple"
                , arbitrary = Api.getArbitraryTuples
                , roundtrip = Api.postRoundtripTuple
                }
    in
    ( { roundtripInt = roundtripInt
      , roundtripTuple = roundtripTuple
      }
    , Cmd.batch
        [ Cmd.map GotRoundtripIntMsg roundtripIntCmd
        , Cmd.map GotRoundtripTupleMsg roundtripTupleCmd
        ]
    )



---- UPDATE ----


type Msg
    = GotRoundtripIntMsg (Roundtrip.Msg Int)
    | GotRoundtripTupleMsg (Roundtrip.Msg ( Int, String ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRoundtripIntMsg subMsg ->
            let
                ( roundtripInt, roundtripIntCmd ) =
                    Roundtrip.update subMsg model.roundtripInt
            in
            ( { model | roundtripInt = roundtripInt }, Cmd.map GotRoundtripIntMsg roundtripIntCmd )

        GotRoundtripTupleMsg subMsg ->
            let
                ( roundtripTuple, roundtripTupleCmd ) =
                    Roundtrip.update subMsg model.roundtripTuple
            in
            ( { model | roundtripTuple = roundtripTuple }, Cmd.map GotRoundtripTupleMsg roundtripTupleCmd )



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ width fill, padding 30 ]
        [ Roundtrip.view model.roundtripInt
        , Roundtrip.view model.roundtripTuple
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = layout [] << view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
