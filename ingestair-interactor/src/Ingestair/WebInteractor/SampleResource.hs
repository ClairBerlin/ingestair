{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Ingestair.WebInteractor.SampleResource
  ( SampleId
  , SampleResource(..)
  , sampleResourceType
  , SampleDocument
  , fromDomain
  , toDomain
  )
where

import           RIO                     hiding ( id )

import qualified Data.UUID                     as UUID
import qualified Data.Scientific               as SC

import qualified Data.Aeson                    as J
import qualified Network.JSONApi               as JA

import qualified Ingestair.Domain.Sample       as DS

type SampleId = Int32

sampleResourceType :: Text
sampleResourceType = "sample"

data SampleResource = SampleResource
    { id :: !(Maybe SampleId)
    , timestamp_s :: !Int32
    , co2_ppm :: !Int16
    , temperature_celsius :: !(Maybe SC.Scientific)
    , rel_humidity_percent :: !(Maybe Int16)
    , measurement_status :: !Text
    , node_id :: !UUID.UUID
    } deriving (Eq, Show, Generic)

instance JA.ResourcefulEntity SampleResource where
  resourceIdentifier = tshow . fromMaybe 0 . id
  resourceType _ = sampleResourceType
  resourceLinks = Just . JA.showLink
  resourceMetaData _ = Nothing
  resourceRelationships _ = Nothing

instance J.ToJSON SampleResource where
  toJSON = J.genericToJSON J.defaultOptions { J.omitNothingFields = True }

instance J.FromJSON SampleResource where
  parseJSON =
    J.genericParseJSON J.defaultOptions { J.omitNothingFields = True }

-- For serving JSON:API documenets
type SampleDocument = JA.Document SampleResource


fromDomain :: DS.Sample -> SampleResource
fromDomain ds = SampleResource (DS.id ds)
                               (DS.timestamp_s ds)
                               (DS.co2_ppm ds)
                               (DS.temperature_celsius ds)
                               (DS.rel_humidity_percent ds)
                               ((DS.toText . DS.measurement_status) ds)
                               (DS.node_id ds)

toDomain :: SampleResource -> Maybe DS.Sample
toDomain sr = DS.mkSample Nothing
                          (timestamp_s sr)
                          (co2_ppm sr)
                          (temperature_celsius sr)
                          (rel_humidity_percent sr)
                          (measurement_status sr)
                          (node_id sr)
