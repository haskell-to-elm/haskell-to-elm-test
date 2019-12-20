{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module API where

import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import Data.Text (Text)
import Data.Time
import Generic.Random (genericArbitraryU)
import Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Language.Haskell.To.Elm
import Servant.API
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances.Text ()

type RoundTrip a = ReqBody '[JSON] a :> Post '[JSON] a

type API = RoundtripAPI :<|> ServantFeatureAPI

type RoundtripAPI
  = "arbitrary" :> "ints" :> Get '[JSON] [Int]
  :<|> "roundtrip" :> "int" :> RoundTrip Int
  :<|> "arbitrary" :> "utctimes" :> Get '[JSON] [UTCTime]
  :<|> "roundtrip" :> "utctime" :> RoundTrip UTCTime
  :<|> "arbitrary" :> "tuples" :> Get '[JSON] [(Int, Text)]
  :<|> "roundtrip" :> "tuple" :> RoundTrip (Int, Text)
  :<|> "arbitrary" :> "adts" :> Get '[JSON] [ADT]
  :<|> "roundtrip" :> "adt" :> RoundTrip ADT
  :<|> "arbitrary" :> "enumadts" :> Get '[JSON] [EnumADT]
  :<|> "roundtrip" :> "enumadt" :> RoundTrip EnumADT
  :<|> "arbitrary" :> "records" :> Get '[JSON] [Record]
  :<|> "roundtrip" :> "record" :> RoundTrip Record
  :<|> "arbitrary" :> "singleconstructors" :> Get '[JSON] [SingleConstructor]
  :<|> "roundtrip" :> "singleconstructor" :> RoundTrip SingleConstructor
  :<|> "arbitrary" :> "singlefieldrecords" :> Get '[JSON] [SingleFieldRecord]
  :<|> "roundtrip" :> "singlefieldrecord" :> RoundTrip SingleFieldRecord
  :<|> "arbitrary" :> "nestedadts" :> Get '[JSON] [NestedADT]
  :<|> "roundtrip" :> "nestedadt" :> RoundTrip NestedADT

type ServantFeatureAPI
    = "header" :> Header "header" Text :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "strictheader" :> Header' '[Required, Strict] "requiredHeader" Text :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "twoheaders" :> Header "optionalHeader" Text :> Header' '[Required, Strict] "requiredHeader" Text :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "paramandbody" :> QueryParam "param" Int :> ReqBody '[JSON] [Text] :> PostNoContent '[JSON] NoContent
 :<|> "requiredparamandbody" :> QueryParam' '[Required, Strict] "param" Int :> ReqBody '[JSON] [Text] :> PostNoContent '[JSON] NoContent
 :<|> "paramsandbody" :> QueryParams "params" Int :> ReqBody '[JSON] Text :> PutNoContent '[JSON] NoContent
 :<|> "capture" :> Capture "id" Int :> DeleteNoContent '[JSON] NoContent
 :<|> "captures" :> CaptureAll "ids" Int :> Get '[JSON] [Int]
 :<|> "static" :> "url" :> Get '[JSON] [Int]

---- Types ----

data ADT = ADTA Int Double | ADTB Text | ADTC
  deriving (GHC.Generic)

data EnumADT = EnumA | EnumB | EnumC
  deriving (GHC.Generic)

data Record = Record { _x :: Int, _y :: Maybe Int }
  deriving (GHC.Generic)

data SingleConstructor = SingleConstructor Bool Int
  deriving (GHC.Generic)

newtype SingleFieldRecord = SingleFieldRecord { _singleField :: Int }
  deriving (GHC.Generic)

data NestedADT
  = FirstConstructor ADT [EnumADT]
  | SecondConstructor ADT [EnumADT]
  deriving (GHC.Generic)

---- ADT ----

instance QuickCheck.Arbitrary ADT where
  arbitrary =
    genericArbitraryU

instance SOP.Generic ADT
instance HasDatatypeInfo ADT

instance HasElmType ADT where
  elmDefinition =
    Just $ deriveElmTypeDefinition @ADT defaultOptions "ADT.ADT"

instance HasElmDecoder Aeson.Value ADT where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @ADT defaultOptions Aeson.defaultOptions "ADT.decode"

instance HasElmEncoder Aeson.Value ADT where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @ADT defaultOptions Aeson.defaultOptions "ADT.encode"

Aeson.deriveJSON Aeson.defaultOptions ''ADT

---- EnumADT ----

instance QuickCheck.Arbitrary EnumADT where
  arbitrary =
    genericArbitraryU

instance SOP.Generic EnumADT
instance HasDatatypeInfo EnumADT

instance HasElmType EnumADT where
  elmDefinition =
    Just $ deriveElmTypeDefinition @EnumADT defaultOptions "EnumADT.EnumADT"

instance HasElmDecoder Aeson.Value EnumADT where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @EnumADT defaultOptions Aeson.defaultOptions "EnumADT.decode"

instance HasElmEncoder Aeson.Value EnumADT where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @EnumADT defaultOptions Aeson.defaultOptions "EnumADT.encode"

Aeson.deriveJSON Aeson.defaultOptions ''EnumADT

---- Record ----

instance QuickCheck.Arbitrary Record where
  arbitrary =
    genericArbitraryU

instance SOP.Generic Record
instance HasDatatypeInfo Record

instance HasElmType Record where
  elmDefinition =
    Just $ deriveElmTypeDefinition @Record defaultOptions { fieldLabelModifier = drop 1 } "Record.Record"

instance HasElmDecoder Aeson.Value Record where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Record defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Record.decode"

instance HasElmEncoder Aeson.Value Record where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Record defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Record.encode"

Aeson.deriveJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } ''Record

---- SingleConstructor ----

instance QuickCheck.Arbitrary SingleConstructor where
  arbitrary =
    genericArbitraryU

instance SOP.Generic SingleConstructor
instance HasDatatypeInfo SingleConstructor

instance HasElmType SingleConstructor where
  elmDefinition =
    Just $ deriveElmTypeDefinition @SingleConstructor defaultOptions "SingleConstructor.SingleConstructor"

instance HasElmDecoder Aeson.Value SingleConstructor where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @SingleConstructor defaultOptions Aeson.defaultOptions "SingleConstructor.decode"

instance HasElmEncoder Aeson.Value SingleConstructor where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @SingleConstructor defaultOptions Aeson.defaultOptions "SingleConstructor.encode"

Aeson.deriveJSON Aeson.defaultOptions ''SingleConstructor

---- SingleFieldRecord ----

instance QuickCheck.Arbitrary SingleFieldRecord where
  arbitrary =
    genericArbitraryU

instance SOP.Generic SingleFieldRecord
instance HasDatatypeInfo SingleFieldRecord

instance HasElmType SingleFieldRecord where
  elmDefinition =
    Just $ deriveElmTypeDefinition @SingleFieldRecord defaultOptions { fieldLabelModifier = drop 1 } "SingleFieldRecord.SingleFieldRecord"

instance HasElmDecoder Aeson.Value SingleFieldRecord where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @SingleFieldRecord defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions "SingleFieldRecord.decode"

instance HasElmEncoder Aeson.Value SingleFieldRecord where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @SingleFieldRecord defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions "SingleFieldRecord.encode"

Aeson.deriveJSON Aeson.defaultOptions ''SingleFieldRecord

---- NestedADT ----

instance QuickCheck.Arbitrary NestedADT where
  arbitrary =
    genericArbitraryU

instance SOP.Generic NestedADT
instance HasDatatypeInfo NestedADT

instance HasElmType NestedADT where
  elmDefinition =
    Just $ deriveElmTypeDefinition @NestedADT defaultOptions { fieldLabelModifier = drop 1 } "NestedADT.NestedADT"

instance HasElmDecoder Aeson.Value NestedADT where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @NestedADT defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions "NestedADT.decode"

instance HasElmEncoder Aeson.Value NestedADT where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @NestedADT defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions "NestedADT.encode"

Aeson.deriveJSON Aeson.defaultOptions ''NestedADT
