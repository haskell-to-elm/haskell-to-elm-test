{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Protolude

import Servant.API

type RoundTrip a = ReqBody '[JSON] a :> Post '[JSON] a

type API
    = "arbitrary" :> "ints" :> Get '[JSON] [Int]
    :<|> "roundtrip" :> "int" :> RoundTrip Int
