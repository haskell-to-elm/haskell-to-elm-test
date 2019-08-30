{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Protolude

import Data.Text (Text)
import Servant.API

type RoundTrip a = ReqBody '[JSON] a :> Post '[JSON] a

type API
    = "arbitrary" :> "ints" :> Get '[JSON] [Int]
    :<|> "roundtrip" :> "int" :> RoundTrip Int
    :<|> "arbitrary" :> "tuples" :> Get '[JSON] [(Int, Text)]
    :<|> "roundtrip" :> "tuple" :> RoundTrip (Int, Text)
