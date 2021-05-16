{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Ingestair.Usecases.IngestUC
  ( ingestSample
  , getSample
  )
where

import           RIO

import qualified Ingestair.Domain.Sample       as DS
import qualified Ingestair.Usecases.SampleRepositoryI
                                               as SRI

-- Call the persistence module to store the sample in the DB.
-- This is a placeholder function. More elaborate business logic would reside here.
ingestSample :: SRI.SampleRepositoryI env => DS.Sample -> RIO env DS.SampleId
ingestSample = SRI.insertSample


-- Query for a specific sample, identified by its ID.
-- Return a sample domain value or propagate any errors.
getSample :: (SRI.SampleRepositoryI env) => DS.SampleId -> RIO env DS.Sample
getSample = SRI.findSampleById
