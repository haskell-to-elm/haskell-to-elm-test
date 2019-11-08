{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where

import qualified Test.QuickCheck as QuickCheck
import Data.Time

instance QuickCheck.Arbitrary UTCTime where
  arbitrary = do
    day <- QuickCheck.choose (1, 29)
    month <- QuickCheck.choose (1, 12)
    year <- QuickCheck.choose (2000, 2030)
    time <- QuickCheck.choose (0, 86401)
    return $ UTCTime (fromGregorian year month day) (fromIntegral (time :: Int))
