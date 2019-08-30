{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude

import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server

import API

main :: IO ()
main =
  run 8081 $ serve (Proxy :: Proxy API) server

server :: Server API
server =
  return 41
  :<|> pure
