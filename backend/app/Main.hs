{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server

import API

main :: IO ()
main = do
  putText "Starting server on port 8081"
  run 8081 $ serve (Proxy :: Proxy API) server

server :: Server API
server =
  return 41
  :<|> pure
