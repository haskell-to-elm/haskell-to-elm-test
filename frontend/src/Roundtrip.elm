module Roundtrip exposing (Model, Msg(..), init, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import WebData exposing (..)



---- MODEL ----


type alias Model a =
    { name : String
    , arbitrary : Cmd (WebResult (List a))
    , roundtrip : a -> Cmd (WebResult a)
    , progress : Progress a
    }


type Progress a
    = FetchingArbitraryValues
    | Testing { current : a, todo : List a }
    | Success
    | HttpFailure ErrorWithMetadata
    | TestFailure { expected : a, actual : a }


init : { name : String, arbitrary : Cmd (WebResult (List a)), roundtrip : a -> Cmd (WebResult a) } -> ( Model a, Cmd (Msg a) )
init { name, arbitrary, roundtrip } =
    ( { name = name
      , arbitrary = arbitrary
      , roundtrip = roundtrip
      , progress = FetchingArbitraryValues
      }
    , Cmd.map GotArbitraryValues arbitrary
    )



---- UPDATE ----


type Msg a
    = GotArbitraryValues (WebResult (List a))
    | GotRoundtripResult (WebResult a)


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg model =
    let
        ( newProgress, cmd ) =
            progress msg model
    in
    ( { model | progress = newProgress }, cmd )


progress : Msg a -> Model a -> ( Progress a, Cmd (Msg a) )
progress msg model =
    let
        nextTest values =
            case values of
                [] ->
                    ( Success, Cmd.none )

                current :: todo ->
                    ( Testing { current = current, todo = todo }
                    , Cmd.map GotRoundtripResult <| model.roundtrip current
                    )
    in
    case ( msg, model.progress ) of
        ( GotArbitraryValues result, FetchingArbitraryValues ) ->
            case result of
                Err e ->
                    ( HttpFailure e, Cmd.none )

                Ok values ->
                    nextTest values

        ( GotArbitraryValues result, _ ) ->
            ( model.progress, Cmd.none )

        ( GotRoundtripResult result, Testing { current, todo } ) ->
            case result of
                Err e ->
                    ( HttpFailure e, Cmd.none )

                Ok value ->
                    if current == value then
                        nextTest todo

                    else
                        ( TestFailure { expected = current, actual = value }, Cmd.none )

        ( GotRoundtripResult result, _ ) ->
            ( model.progress, Cmd.none )



---- VIEW ----


view : Model a -> Element msg
view model =
    let
        ( backgroundColor, explanation ) =
            case model.progress of
                FetchingArbitraryValues ->
                    ( rgb 1 1 1, "fetching test values" )

                Testing _ ->
                    ( rgb 1 1 0.5, "testing …" )

                Success ->
                    ( rgb 0.5 1 0.5, "success" )

                HttpFailure _ ->
                    ( rgb 1 0.5 0.5, "HTTP failure" )

                TestFailure _ ->
                    ( rgb 1 0.5 0.5, "failure" )
    in
    column
        [ paddingXY 20 10
        , Background.color backgroundColor
        , width <| px 200
        ]
        [ el [ width fill, Font.center ] <| text model.name
        , el [ width fill, Font.center, Font.color (rgb 0.5 0.5 0.5), Font.size 15 ] <| text <| "(" ++ explanation ++ ")"
        ]
