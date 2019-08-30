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

type API
    = "arbitrary" :> "ints" :> Get '[JSON] [Int]
    :<|> "roundtrip" :> "int" :> RoundTrip Int
    :<|> "arbitrary" :> "tuples" :> Get '[JSON] [(Int, Text)]
    :<|> "roundtrip" :> "tuple" :> RoundTrip (Int, Text)
    :<|> "arbitrary" :> "adts" :> Get '[JSON] [ADT]
    :<|> "roundtrip" :> "adt" :> RoundTrip ADT
    :<|> "arbitrary" :> "enumadts" :> Get '[JSON] [EnumADT]
    :<|> "roundtrip" :> "enumadt" :> RoundTrip EnumADT

---- Types ----

data ADT = ADTA Int Double | ADTB Text | ADTC
  deriving (GHC.Generic)

data EnumADT = EnumA | EnumB | EnumC
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


-- data Test2 = C | D
--   deriving (GHC.Generic)

-- instance SOP.Generic Test2
-- instance HasDatatypeInfo Test2

-- instance HasElmDefinition Test2 where
--   elmDefinition = deriveElmTypeDefinition @Test2 defaultOptions { fieldLabelModifier = drop 1 } "Test2.Test2"

-- instance HasElmDecoderDefinition Aeson.Value Test2 where
--   elmDecoderDefinition = deriveElmJSONDecoder @Test2 defaultOptions Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Test2.decode"

-- instance HasElmEncoderDefinition Aeson.Value Test2 where
--   elmEncoderDefinition = deriveElmJSONEncoder @Test2 defaultOptions Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Test2.encode"

-- instance HasElmType Test2 where

-- data Rec = Rec { _x :: Int, _y :: Maybe Int }
--   deriving (GHC.Generic)

-- instance SOP.Generic Rec
-- instance HasDatatypeInfo Rec

-- instance HasElmDefinition Rec where
--   elmDefinition = deriveElmTypeDefinition @Rec defaultOptions { fieldLabelModifier = drop 1 } "Rec.Rec"
-- instance HasElmType Rec where

-- instance HasElmDecoderDefinition Aeson.Value Rec where
--   elmDecoderDefinition = deriveElmJSONDecoder @Rec defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Rec.decode"

-- instance HasElmEncoderDefinition Aeson.Value Rec where
--   elmEncoderDefinition = deriveElmJSONEncoder @Rec defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Rec.encode"

-- instance HasElmDecoder Aeson.Value Rec

-- data SingleConstructor = SingleConstructor Bool Int
--   deriving (GHC.Generic)

-- instance SOP.Generic SingleConstructor
-- instance HasDatatypeInfo SingleConstructor

-- instance Aeson.ToJSON SingleConstructor where
--   toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- instance HasElmDefinition SingleConstructor where
--   elmDefinition = deriveElmTypeDefinition @SingleConstructor defaultOptions { fieldLabelModifier = drop 1 } "SingleConstructor.SingleConstructor"

-- instance HasElmDecoderDefinition Aeson.Value SingleConstructor where
--   elmDecoderDefinition = deriveElmJSONDecoder @SingleConstructor defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "SingleConstructor.decode"

-- instance HasElmEncoderDefinition Aeson.Value SingleConstructor where
--   elmEncoderDefinition = deriveElmJSONEncoder @SingleConstructor defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "SingleConstructor.encode"

-- instance HasElmType SingleConstructor where

-- data SingleFieldRecord = SingleFieldRecord { _singleField :: Int }
--   deriving (GHC.Generic)

-- instance SOP.Generic SingleFieldRecord
-- instance HasDatatypeInfo SingleFieldRecord

-- instance Aeson.ToJSON SingleFieldRecord where
--   toEncoding = Aeson.genericToEncoding Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1, Aeson.unwrapUnaryRecords = False }
--   toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1, Aeson.unwrapUnaryRecords = False }

-- instance HasElmDefinition SingleFieldRecord where
--   elmDefinition = deriveElmTypeDefinition @SingleFieldRecord defaultOptions { fieldLabelModifier = drop 1 } "SingleFieldRecord.SingleFieldRecord"

-- instance HasElmDecoderDefinition Aeson.Value SingleFieldRecord where
--   elmDecoderDefinition = deriveElmJSONDecoder @SingleFieldRecord defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1, Aeson.unwrapUnaryRecords = False } "SingleFieldRecord.decode"

-- instance HasElmEncoderDefinition Aeson.Value SingleFieldRecord where
--   elmEncoderDefinition = deriveElmJSONEncoder @SingleFieldRecord defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1, Aeson.unwrapUnaryRecords = False } "SingleFieldRecord.encode"

-- instance HasElmType SingleFieldRecord where

-- everything :: forall t. (HasElmDefinition t, HasElmEncoderDefinition Aeson.Value t, HasElmDecoderDefinition Aeson.Value t) => [Definition]
-- everything =
--   [ elmDefinition @t
--   , elmEncoderDefinition @Aeson.Value @t
--   , elmDecoderDefinition @Aeson.Value @t
--   ]

-- test = Pretty.modules $
--   concat $
--   [ everything @Test
--   , everything @Test2
--   , everything @Rec
--   , everything @SingleConstructor
--   , everything @SingleFieldRecord
--   ]
