module Main exposing (main)

import ADT
import Api
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import EnumADT
import Http
import MyList
import MyPair
import NestedADT
import Record
import RemoteData exposing (RemoteData)
import Roundtrip
import SingleConstructor
import SingleFieldRecord
import Time
import WebData exposing (..)



---- MODEL ----


type alias Model =
    { roundtripInt : Roundtrip.Model Int
    , roundtripUTCTime : Roundtrip.Model Time.Posix
    , roundtripTuple : Roundtrip.Model ( Int, String )
    , roundtripADT : Roundtrip.Model ADT.ADT
    , roundtripEnumADT : Roundtrip.Model EnumADT.EnumADT
    , roundtripRecord : Roundtrip.Model Record.Record
    , roundtripSingleConstructor : Roundtrip.Model SingleConstructor.SingleConstructor
    , roundtripSingleFieldRecord : Roundtrip.Model SingleFieldRecord.SingleFieldRecord
    , roundtripNestedADT : Roundtrip.Model NestedADT.NestedADT
    , roundtripList : Roundtrip.Model (MyList.List Record.Record)
    , roundtripPair : Roundtrip.Model (MyPair.Pair SingleConstructor.SingleConstructor NestedADT.NestedADT)
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

        ( roundtripUTCTime, roundtripUTCTimeCmd ) =
            Roundtrip.init
                { name = "UTCTime"
                , arbitrary = Api.getArbitraryUtctimes
                , roundtrip = Api.postRoundtripUtctime
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

        ( roundtripSingleFieldRecord, roundtripSingleFieldRecordCmd ) =
            Roundtrip.init
                { name = "Single field record"
                , arbitrary = Api.getArbitrarySinglefieldrecords
                , roundtrip = Api.postRoundtripSinglefieldrecord
                }

        ( roundtripNestedADT, roundtripNestedADTCmd ) =
            Roundtrip.init
                { name = "Nested ADT"
                , arbitrary = Api.getArbitraryNestedadts
                , roundtrip = Api.postRoundtripNestedadt
                }

        ( roundtripList, roundtripListCmd ) =
            Roundtrip.init
                { name = "List"
                , arbitrary = Api.getArbitraryLists
                , roundtrip = Api.postRoundtripList
                }

        ( roundtripPair, roundtripPairCmd ) =
            Roundtrip.init
                { name = "Pair"
                , arbitrary = Api.getArbitraryPairs
                , roundtrip = Api.postRoundtripPair
                }
    in
    ( { roundtripInt = roundtripInt
      , roundtripUTCTime = roundtripUTCTime
      , roundtripTuple = roundtripTuple
      , roundtripADT = roundtripADT
      , roundtripEnumADT = roundtripEnumADT
      , roundtripRecord = roundtripRecord
      , roundtripSingleConstructor = roundtripSingleConstructor
      , roundtripSingleFieldRecord = roundtripSingleFieldRecord
      , roundtripNestedADT = roundtripNestedADT
      , roundtripList = roundtripList
      , roundtripPair = roundtripPair
      }
    , Cmd.batch
        [ Cmd.map GotRoundtripIntMsg roundtripIntCmd
        , Cmd.map GotRoundtripUTCTimeMsg roundtripUTCTimeCmd
        , Cmd.map GotRoundtripTupleMsg roundtripTupleCmd
        , Cmd.map GotRoundtripADTMsg roundtripADTCmd
        , Cmd.map GotRoundtripEnumADTMsg roundtripEnumADTCmd
        , Cmd.map GotRoundtripRecordMsg roundtripRecordCmd
        , Cmd.map GotRoundtripSingleConstructorMsg roundtripSingleConstructorCmd
        , Cmd.map GotRoundtripSingleFieldRecordMsg roundtripSingleFieldRecordCmd
        , Cmd.map GotRoundtripNestedADTMsg roundtripNestedADTCmd
        , Cmd.map GotRoundtripListMsg roundtripListCmd
        , Cmd.map GotRoundtripPairMsg roundtripPairCmd
        ]
    )



---- UPDATE ----


type Msg
    = GotRoundtripIntMsg (Roundtrip.Msg Int)
    | GotRoundtripUTCTimeMsg (Roundtrip.Msg Time.Posix)
    | GotRoundtripTupleMsg (Roundtrip.Msg ( Int, String ))
    | GotRoundtripADTMsg (Roundtrip.Msg ADT.ADT)
    | GotRoundtripEnumADTMsg (Roundtrip.Msg EnumADT.EnumADT)
    | GotRoundtripRecordMsg (Roundtrip.Msg Record.Record)
    | GotRoundtripSingleConstructorMsg (Roundtrip.Msg SingleConstructor.SingleConstructor)
    | GotRoundtripSingleFieldRecordMsg (Roundtrip.Msg SingleFieldRecord.SingleFieldRecord)
    | GotRoundtripNestedADTMsg (Roundtrip.Msg NestedADT.NestedADT)
    | GotRoundtripListMsg (Roundtrip.Msg (MyList.List Record.Record))
    | GotRoundtripPairMsg (Roundtrip.Msg (MyPair.Pair SingleConstructor.SingleConstructor NestedADT.NestedADT))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRoundtripIntMsg subMsg ->
            let
                ( roundtrip, roundtripCmd ) =
                    Roundtrip.update subMsg model.roundtripInt
            in
            ( { model | roundtripInt = roundtrip }, Cmd.map GotRoundtripIntMsg roundtripCmd )

        GotRoundtripUTCTimeMsg subMsg ->
            let
                ( roundtrip, roundtripCmd ) =
                    Roundtrip.update subMsg model.roundtripUTCTime
            in
            ( { model | roundtripUTCTime = roundtrip }, Cmd.map GotRoundtripUTCTimeMsg roundtripCmd )

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

        GotRoundtripSingleFieldRecordMsg subMsg ->
            let
                ( roundtrip, roundtripCmd ) =
                    Roundtrip.update subMsg model.roundtripSingleFieldRecord
            in
            ( { model | roundtripSingleFieldRecord = roundtrip }, Cmd.map GotRoundtripSingleFieldRecordMsg roundtripCmd )

        GotRoundtripNestedADTMsg subMsg ->
            let
                ( roundtrip, roundtripCmd ) =
                    Roundtrip.update subMsg model.roundtripNestedADT
            in
            ( { model | roundtripNestedADT = roundtrip }, Cmd.map GotRoundtripNestedADTMsg roundtripCmd )

        GotRoundtripListMsg subMsg ->
            let
                ( roundtrip, roundtripCmd ) =
                    Roundtrip.update subMsg model.roundtripList
            in
            ( { model | roundtripList = roundtrip }, Cmd.map GotRoundtripListMsg roundtripCmd )

        GotRoundtripPairMsg subMsg ->
            let
                ( roundtrip, roundtripCmd ) =
                    Roundtrip.update subMsg model.roundtripPair
            in
            ( { model | roundtripPair = roundtrip }, Cmd.map GotRoundtripPairMsg roundtripCmd )



---- VIEW ----


view : Model -> Element Msg
view model =
    row [ width fill ]
        [ column [ width fill, padding 30, spacing 10 ]
            [ el [ width <| px 200, Font.center ] <| text "Roundtrip tests"
            , Roundtrip.view model.roundtripInt
            , Roundtrip.view model.roundtripUTCTime
            , Roundtrip.view model.roundtripTuple
            , Roundtrip.view model.roundtripADT
            , Roundtrip.view model.roundtripEnumADT
            , Roundtrip.view model.roundtripRecord
            , Roundtrip.view model.roundtripSingleConstructor
            , Roundtrip.view model.roundtripSingleFieldRecord
            , Roundtrip.view model.roundtripNestedADT
            , Roundtrip.view model.roundtripList
            , Roundtrip.view model.roundtripPair
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
