{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ingestair.WebInteractor.IngestEndpoint
  ( ingestSample
  , getSample
  )
where

import           RIO
import qualified RIO.Text                      as T

import qualified Servant                       as SV
import qualified Network.JSONApi               as JA

import qualified Ingestair.Domain.Sample       as DS

import qualified Ingestair.WebInteractor.SampleRequest
                                               as SQ
import qualified Ingestair.WebInteractor.SampleResource
                                               as SR
import qualified Ingestair.Usecases.IngestUC
                                               as IUC
import qualified Ingestair.Usecases.SampleRepositoryI
                                               as SRI


-- Parse an incoming sample request, convert it into the proper domain representation,
-- and call the controller to persist it.
-- Return the ID under which the sample can be retrieved.
ingestSample
  :: (HasLogFunc env, SRI.SampleRepositoryI env)
  => SQ.SampleRequestDocument
  -> RIO env SR.SampleId
ingestSample (SQ.SampleRequestDocument requestData) = do
  logDebug "Parsing incoming sample ingestion request..."
  let sampleResource = SQ.data_attributes requestData
      jsonApiType    = (T.toLower . SQ.data_type) requestData
  maybeSample <- -- Ensure correct JSON:API type.
                 if jsonApiType == SR.sampleResourceType
    then pure $ SR.toDomain sampleResource -- Parse data and convert to domain type.
    else throwIO SV.err400
  case maybeSample of
    Just sample -> IUC.ingestSample sample
    Nothing     -> throwIO SV.err400


-- Parse the query for a specific sample identified by its ID.
-- Call the correponding controller to retrieve it.
-- Map the returned domain type onto the resource provided at the API.
getSample
  :: (HasLogFunc env, SRI.SampleRepositoryI env)
  => SR.SampleId
  -> RIO env SR.SampleDocument
getSample sId = do
  logDebug "Retrieving a sample."
  sample <- IUC.getSample sId `catch` handler
  return $ (JA.mkSimpleDocument . SR.fromDomain) sample

 where
  handler :: SRI.NotFoundError -> RIO env DS.Sample
  handler _ = throwIO SV.err404
