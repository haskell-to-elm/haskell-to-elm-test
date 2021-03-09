{-# LANGUAGE NoImplicitPrelude #-}
module AesonOptions where

import Protolude hiding (all)

import qualified Data.Aeson as Aeson
import qualified Data.List as List

all :: [Aeson.Options]
all = do
  fieldLabelModifier_ <- [identity, ("field_prefix_" <>)]
  constructorTagModifier_ <- [identity, ("constructor_tag_prefix_" <>)]
  allNullaryToStringTag_ <- [False, True]
  omitNothingFields_ <- [False, True]
  sumEncoding_ <-
    [ Aeson.TaggedObject
      { Aeson.tagFieldName = "tag_field_name"
      , Aeson.contentsFieldName = "contents_field_name"
      }
    , Aeson.UntaggedValue
    , Aeson.ObjectWithSingleField
    , Aeson.TwoElemArray
    ]
  unwrapUnaryRecords_ <- [False, True]
  tagSingleConstructors_ <- [False, True]
  pure
    Aeson.defaultOptions
      { Aeson.fieldLabelModifier = fieldLabelModifier_
      , Aeson.constructorTagModifier = constructorTagModifier_
      , Aeson.allNullaryToStringTag = allNullaryToStringTag_
      , Aeson.omitNothingFields = omitNothingFields_
      , Aeson.sumEncoding = sumEncoding_
      , Aeson.unwrapUnaryRecords = unwrapUnaryRecords_
      , Aeson.tagSingleConstructors = tagSingleConstructors_
      }

supported :: [Aeson.Options]
supported = do
  fieldLabelModifier_ <- [identity, ("field_prefix_" <>)]
  constructorTagModifier_ <- [identity, ("constructor_tag_prefix_" <>)]
  allNullaryToStringTag_ <- [False, True]
  omitNothingFields_ <- [False, True]
  pure
    Aeson.defaultOptions
      { Aeson.fieldLabelModifier = fieldLabelModifier_
      , Aeson.constructorTagModifier = constructorTagModifier_
      , Aeson.allNullaryToStringTag = allNullaryToStringTag_
      , Aeson.omitNothingFields = omitNothingFields_
      }

aesonOptions :: Aeson.Options
aesonOptions =
  supported List.!! 0
