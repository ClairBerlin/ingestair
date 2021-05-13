{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Ingestair.Controller.Ingest
  ( ingestSample
  , getSample
  )
where

import           RIO

import qualified Servant                       as SV

import qualified Ingestair.Domain.Sample       as DS
import qualified Ingestair.Api.Api             as Api
import qualified Ingestair.Controller.SampleRepositoryI
                                               as SRI

-- Validate an incoming sample request and persist it.
-- Return the ID under which the sample can be retrieved.
ingestSample
  :: (HasLogFunc env, SRI.SampleRepositoryI env)
  => Api.SampleRequest
  -> RIO env Api.SampleId
ingestSample sampleReq = do
  logDebug "Ingesting a sample."
  let maybeSample = DS.mkSample (Api.timestamp_s sampleReq)
                                (Api.co2_ppm sampleReq)
                                (Api.temperature_celsius sampleReq)
                                (Api.rel_humidity_percent sampleReq)
                                (Api.measurement_status sampleReq)
                                (Api.node_id sampleReq)
  case maybeSample of
    Just sample -> SRI.insertSample sample
    Nothing     -> throwIO SV.err400


-- Query for a specific sample, identified by its ID.
getSample
  :: (HasLogFunc env, SRI.SampleRepositoryI env)
  => Api.SampleId
  -> RIO env DS.Sample
getSample sId = do
  logDebug "Retrieving a sample."
  SRI.findSampleById sId `catch` handler
 where
  handler :: SRI.NotFoundError -> RIO env DS.Sample
  handler _ = throwIO SV.err404
