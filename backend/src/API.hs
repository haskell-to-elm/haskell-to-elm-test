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
import Generic.Random (genericArbitraryU)
import Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Language.Haskell.To.Elm
import Orphans ()
import Servant.API
import qualified Test.QuickCheck as QuickCheck

type RoundTrip a = ReqBody '[JSON] a :> Post '[JSON] a

type API = RoundtripAPI :<|> ServantFeatureAPI

type RoundtripAPI
  = "arbitrary" :> "ints" :> Get '[JSON] [Int]
  :<|> "roundtrip" :> "int" :> RoundTrip Int
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

type ServantFeatureAPI
    = "header" :> Header "header" Text :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "strictheader" :> Header' '[Required, Strict] "requiredHeader" Text :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "twoheaders" :> Header "optionalHeader" Text :> Header' '[Required, Strict] "requiredHeader" Text :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "paramandbody" :> QueryParam "param" Int :> ReqBody '[JSON] [Text] :> Post '[JSON] NoContent
 :<|> "requiredparamandbody" :> QueryParam' '[Required, Strict] "param" Int :> ReqBody '[JSON] [Text] :> Post '[JSON] NoContent
 :<|> "paramsandbody" :> QueryParams "params" Int :> ReqBody '[JSON] Text :> Put '[JSON] NoContent
 :<|> "capture" :> Capture "id" Int :> Delete '[JSON] NoContent
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

---- ADT ----

instance QuickCheck.Arbitrary ADT where
  arbitrary =
    genericArbitraryU

instance SOP.Generic ADT
instance HasDatatypeInfo ADT

instance HasElmDefinition ADT where
  elmDefinition =
    deriveElmTypeDefinition @ADT defaultOptions "ADT.ADT"

instance HasElmDecoderDefinition Aeson.Value ADT where
  elmDecoderDefinition =
    deriveElmJSONDecoder @ADT defaultOptions Aeson.defaultOptions "ADT.decode"

instance HasElmEncoderDefinition Aeson.Value ADT where
  elmEncoderDefinition =
    deriveElmJSONEncoder @ADT defaultOptions Aeson.defaultOptions "ADT.encode"

instance HasElmType ADT where
instance HasElmEncoder Aeson.Value ADT where
instance HasElmDecoder Aeson.Value ADT where

Aeson.deriveJSON Aeson.defaultOptions ''ADT

---- EnumADT ----

instance QuickCheck.Arbitrary EnumADT where
  arbitrary =
    genericArbitraryU

instance SOP.Generic EnumADT
instance HasDatatypeInfo EnumADT

instance HasElmDefinition EnumADT where
  elmDefinition =
    deriveElmTypeDefinition @EnumADT defaultOptions "EnumADT.EnumADT"

instance HasElmDecoderDefinition Aeson.Value EnumADT where
  elmDecoderDefinition =
    deriveElmJSONDecoder @EnumADT defaultOptions Aeson.defaultOptions "EnumADT.decode"

instance HasElmEncoderDefinition Aeson.Value EnumADT where
  elmEncoderDefinition =
    deriveElmJSONEncoder @EnumADT defaultOptions Aeson.defaultOptions "EnumADT.encode"

instance HasElmType EnumADT where
instance HasElmEncoder Aeson.Value EnumADT where
instance HasElmDecoder Aeson.Value EnumADT where

Aeson.deriveJSON Aeson.defaultOptions ''EnumADT

---- Record ----

instance QuickCheck.Arbitrary Record where
  arbitrary =
    genericArbitraryU

instance SOP.Generic Record
instance HasDatatypeInfo Record

instance HasElmDefinition Record where
  elmDefinition =
    deriveElmTypeDefinition @Record defaultOptions { fieldLabelModifier = drop 1 } "Record.Record"

instance HasElmDecoderDefinition Aeson.Value Record where
  elmDecoderDefinition =
    deriveElmJSONDecoder @Record defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Record.decode"

instance HasElmEncoderDefinition Aeson.Value Record where
  elmEncoderDefinition =
    deriveElmJSONEncoder @Record defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Record.encode"

instance HasElmType Record where
instance HasElmDecoder Aeson.Value Record
instance HasElmEncoder Aeson.Value Record

Aeson.deriveJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } ''Record

---- SingleConstructor ----

instance QuickCheck.Arbitrary SingleConstructor where
  arbitrary =
    genericArbitraryU

instance SOP.Generic SingleConstructor
instance HasDatatypeInfo SingleConstructor

instance HasElmDefinition SingleConstructor where
  elmDefinition =
    deriveElmTypeDefinition @SingleConstructor defaultOptions "SingleConstructor.SingleConstructor"

instance HasElmDecoderDefinition Aeson.Value SingleConstructor where
  elmDecoderDefinition =
    deriveElmJSONDecoder @SingleConstructor defaultOptions Aeson.defaultOptions "SingleConstructor.decode"

instance HasElmEncoderDefinition Aeson.Value SingleConstructor where
  elmEncoderDefinition =
    deriveElmJSONEncoder @SingleConstructor defaultOptions Aeson.defaultOptions "SingleConstructor.encode"

instance HasElmType SingleConstructor where
instance HasElmDecoder Aeson.Value SingleConstructor
instance HasElmEncoder Aeson.Value SingleConstructor

Aeson.deriveJSON Aeson.defaultOptions ''SingleConstructor

---- SingleFieldRecord ----

instance QuickCheck.Arbitrary SingleFieldRecord where
  arbitrary =
    genericArbitraryU

instance SOP.Generic SingleFieldRecord
instance HasDatatypeInfo SingleFieldRecord

instance HasElmDefinition SingleFieldRecord where
  elmDefinition =
    deriveElmTypeDefinition @SingleFieldRecord defaultOptions { fieldLabelModifier = drop 1 } "SingleFieldRecord.SingleFieldRecord"

instance HasElmDecoderDefinition Aeson.Value SingleFieldRecord where
  elmDecoderDefinition =
    deriveElmJSONDecoder @SingleFieldRecord defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions "SingleFieldRecord.decode"

instance HasElmEncoderDefinition Aeson.Value SingleFieldRecord where
  elmEncoderDefinition =
    deriveElmJSONEncoder @SingleFieldRecord defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions "SingleFieldRecord.encode"

instance HasElmType SingleFieldRecord where
instance HasElmDecoder Aeson.Value SingleFieldRecord
instance HasElmEncoder Aeson.Value SingleFieldRecord

Aeson.deriveJSON Aeson.defaultOptions ''SingleFieldRecord
