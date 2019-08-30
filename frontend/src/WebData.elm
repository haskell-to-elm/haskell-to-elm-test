module WebData exposing (ErrorWithMetadata, WebData, WebResult)

import Http
import RemoteData exposing (RemoteData)


type alias ErrorWithMetadata =
    ( Http.Error, Maybe { metadata : Http.Metadata, body : String } )


type alias WebData a =
    RemoteData ErrorWithMetadata a


type alias WebResult a =
    Result ErrorWithMetadata a
