{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where

import Protolude

import Test.QuickCheck
import qualified Data.Text as Text

instance Arbitrary Text where
  arbitrary =
    Text.pack <$> arbitrary
