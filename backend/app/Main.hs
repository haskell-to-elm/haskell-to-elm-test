{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Servant.API
import Servant.Server
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances.Text ()

import Orphans ()
import API

main :: IO ()
main = do
  putText "Starting server on port 8081"
  run 8081 $
    logStdout $
    cors
      (const $ Just simpleCorsResourcePolicy
        { corsRequestHeaders = "authorization" : simpleHeaders
        }
      ) $
    serve (Proxy :: Proxy RoundtripAPI) server

server :: Server RoundtripAPI
server =
  arbitrary :<|> pure :<|>
  arbitrary :<|> pure :<|>
  arbitrary :<|> pure :<|>
  arbitrary :<|> pure :<|>
  arbitrary :<|> pure :<|>
  arbitrary :<|> pure :<|>
  arbitrary :<|> pure :<|>
  arbitrary :<|> pure :<|>
  arbitrary :<|> pure :<|>
  arbitrary :<|> pure :<|>
  arbitrary :<|> pure :<|>
  arbitrary :<|> pure
  where
    arbitrary :: (MonadIO m, QuickCheck.Arbitrary a) => m [a]
    arbitrary = 
      liftIO $
        QuickCheck.generate $
        sequence
          [ QuickCheck.resize n QuickCheck.arbitrary | n <- [0..100] ]
