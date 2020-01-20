{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Protolude hiding ((<.>), moduleName)

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm
import Servant.To.Elm
import System.Directory
import System.FilePath

import API

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outDir] ->
      forM_ modules $ \(moduleName, moduleBody) -> do
        let
          fileName =
            joinPath (outDir : fmap toS moduleName) <.> "elm"
        createDirectoryIfMissing True $ takeDirectory fileName
        writeFile fileName $ show moduleBody

    _ -> do
      putText "No command-line argument given to specify the out directory; printing instead"
      forM_ modules $ \(moduleName, moduleBody) -> do
        putText $ Text.intercalate "." moduleName <> ":"
        putText $ show moduleBody
  where
    modules =
      HashMap.toList $
      Pretty.modules $ fmap Simplification.simplifyDefinition $
        map (elmEndpointDefinition "Config.api" ["Api"]) (elmEndpoints @API) <>
        mconcat
          [ jsonDefinitions @ADT
          , jsonDefinitions @EnumADT
          , jsonDefinitions @Record
          , jsonDefinitions @SingleConstructor
          , jsonDefinitions @SingleFieldRecord
          , jsonDefinitions @NestedADT
          ]
