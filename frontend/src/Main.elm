module Main exposing (main)

import ADT
import Api
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import EnumADT
import Http
import Record
import RemoteData exposing (RemoteData)
import Roundtrip
import SingleConstructor
import WebData exposing (..)



---- MODEL ----


type alias Model =
    { roundtripInt : Roundtrip.Model Int
    , roundtripTuple : Roundtrip.Model ( Int, String )
    , roundtripADT : Roundtrip.Model ADT.ADT
    , roundtripEnumADT : Roundtrip.Model EnumADT.EnumADT
    , roundtripRecord : Roundtrip.Model Record.Record
    , roundtripSingleConstructor : Roundtrip.Model SingleConstructor.SingleConstructor
    }


init : ( Model, Cmd Msg )
init =
    let
        ( roundtripInt, roundtripIntCmd ) =
            Roundtrip.init
                { name = "Int"
                , arbitrary = Api.getArbitraryInts
                , roundtrip = Api.postRoundtripInt
                }

        ( roundtripTuple, roundtripTupleCmd ) =
            Roundtrip.init
                { name = "Tuple"
                , arbitrary = Api.getArbitraryTuples
                , roundtrip = Api.postRoundtripTuple
                }

        ( roundtripADT, roundtripADTCmd ) =
            Roundtrip.init
                { name = "ADT"
                , arbitrary = Api.getArbitraryAdts
                , roundtrip = Api.postRoundtripAdt
                }

        ( roundtripEnumADT, roundtripEnumADTCmd ) =
            Roundtrip.init
                { name = "Enum ADT"
                , arbitrary = Api.getArbitraryEnumadts
                , roundtrip = Api.postRoundtripEnumadt
                }

        ( roundtripRecord, roundtripRecordCmd ) =
            Roundtrip.init
                { name = "Record"
                , arbitrary = Api.getArbitraryRecords
                , roundtrip = Api.postRoundtripRecord
                }

        ( roundtripSingleConstructor, roundtripSingleConstructorCmd ) =
            Roundtrip.init
                { name = "Single constructor"
                , arbitrary = Api.getArbitrarySingleconstructors
                , roundtrip = Api.postRoundtripSingleconstructor
                }
    in
    ( { roundtripInt = roundtripInt
      , roundtripTuple = roundtripTuple
      , roundtripADT = roundtripADT
      , roundtripEnumADT = roundtripEnumADT
      , roundtripRecord = roundtripRecord
      , roundtripSingleConstructor = roundtripSingleConstructor
      }
    , Cmd.batch
        [ Cmd.map GotRoundtripIntMsg roundtripIntCmd
        , Cmd.map GotRoundtripTupleMsg roundtripTupleCmd
        , Cmd.map GotRoundtripADTMsg roundtripADTCmd
        , Cmd.map GotRoundtripEnumADTMsg roundtripEnumADTCmd
        , Cmd.map GotRoundtripRecordMsg roundtripRecordCmd
        , Cmd.map GotRoundtripSingleConstructorMsg roundtripSingleConstructorCmd
        ]
    )



---- UPDATE ----


type Msg
    = GotRoundtripIntMsg (Roundtrip.Msg Int)
    | GotRoundtripTupleMsg (Roundtrip.Msg ( Int, String ))
    | GotRoundtripADTMsg (Roundtrip.Msg ADT.ADT)
    | GotRoundtripEnumADTMsg (Roundtrip.Msg EnumADT.EnumADT)
    | GotRoundtripRecordMsg (Roundtrip.Msg Record.Record)
    | GotRoundtripSingleConstructorMsg (Roundtrip.Msg SingleConstructor.SingleConstructor)


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

        GotRoundtripRecordMsg subMsg ->
            let
                ( roundtrip, roundtripCmd ) =
                    Roundtrip.update subMsg model.roundtripRecord
            in
            ( { model | roundtripRecord = roundtrip }, Cmd.map GotRoundtripRecordMsg roundtripCmd )

        GotRoundtripSingleConstructorMsg subMsg ->
            let
                ( roundtrip, roundtripCmd ) =
                    Roundtrip.update subMsg model.roundtripSingleConstructor
            in
            ( { model | roundtripSingleConstructor = roundtrip }, Cmd.map GotRoundtripSingleConstructorMsg roundtripCmd )



---- VIEW ----


view : Model -> Element Msg
view model =
    row [ width fill ]
        [ column [ width fill, padding 30, spacing 10 ]
            [ el [ width <| px 200, Font.center ] <| text "Roundtrip tests"
            , Roundtrip.view model.roundtripInt
            , Roundtrip.view model.roundtripTuple
            , Roundtrip.view model.roundtripADT
            , Roundtrip.view model.roundtripEnumADT
            , Roundtrip.view model.roundtripRecord
            , Roundtrip.view model.roundtripSingleConstructor
            ]
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
