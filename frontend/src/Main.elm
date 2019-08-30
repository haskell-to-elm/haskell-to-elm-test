module Main exposing (main)

import ADT
import Api
import Browser
import Element exposing (..)
import Element.Background as Background
import EnumADT
import Http
import RemoteData exposing (RemoteData)
import Roundtrip
import WebData exposing (..)



---- MODEL ----


type alias Model =
    { roundtripInt : Roundtrip.Model Int
    , roundtripTuple : Roundtrip.Model ( Int, String )
    , roundtripADT : Roundtrip.Model ADT.ADT
    , roundtripEnumADT : Roundtrip.Model EnumADT.EnumADT
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

        ( roundtripADT, roundtripADTCmd ) =
            Roundtrip.init
                { name = "roundtripADT"
                , arbitrary = Api.getArbitraryAdts
                , roundtrip = Api.postRoundtripAdt
                }

        ( roundtripEnumADT, roundtripEnumADTCmd ) =
            Roundtrip.init
                { name = "roundtripEnumADT"
                , arbitrary = Api.getArbitraryEnumadts
                , roundtrip = Api.postRoundtripEnumadt
                }
    in
    ( { roundtripInt = roundtripInt
      , roundtripTuple = roundtripTuple
      , roundtripADT = roundtripADT
      , roundtripEnumADT = roundtripEnumADT
      }
    , Cmd.batch
        [ Cmd.map GotRoundtripIntMsg roundtripIntCmd
        , Cmd.map GotRoundtripTupleMsg roundtripTupleCmd
        , Cmd.map GotRoundtripADTMsg roundtripADTCmd
        , Cmd.map GotRoundtripEnumADTMsg roundtripEnumADTCmd
        ]
    )



---- UPDATE ----


type Msg
    = GotRoundtripIntMsg (Roundtrip.Msg Int)
    | GotRoundtripTupleMsg (Roundtrip.Msg ( Int, String ))
    | GotRoundtripADTMsg (Roundtrip.Msg ADT.ADT)
    | GotRoundtripEnumADTMsg (Roundtrip.Msg EnumADT.EnumADT)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRoundtripIntMsg subMsg ->
            let
                ( roundtrip, roundtripCmd ) =
                    Roundtrip.update subMsg model.roundtripInt
            in
            ( { model | roundtripInt = roundtrip }, Cmd.map GotRoundtripIntMsg roundtripCmd )

        GotRoundtripTupleMsg subMsg ->
            let
                ( roundtrip, roundtripCmd ) =
                    Roundtrip.update subMsg model.roundtripTuple
            in
            ( { model | roundtripTuple = roundtrip }, Cmd.map GotRoundtripTupleMsg roundtripCmd )

        GotRoundtripADTMsg subMsg ->
            let
                ( roundtrip, roundtripCmd ) =
                    Roundtrip.update subMsg model.roundtripADT
            in
            ( { model | roundtripADT = roundtrip }, Cmd.map GotRoundtripADTMsg roundtripCmd )

        GotRoundtripEnumADTMsg subMsg ->
            let
                ( roundtrip, roundtripCmd ) =
                    Roundtrip.update subMsg model.roundtripEnumADT
            in
            ( { model | roundtripEnumADT = roundtrip }, Cmd.map GotRoundtripEnumADTMsg roundtripCmd )



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ width fill, padding 30, spacing 10 ]
        [ Roundtrip.view model.roundtripInt
        , Roundtrip.view model.roundtripTuple
        , Roundtrip.view model.roundtripADT
        , Roundtrip.view model.roundtripEnumADT
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
