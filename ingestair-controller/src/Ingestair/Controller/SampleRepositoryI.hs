{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ingestair.Controller.SampleRepositoryI
  ( SampleId
  , SampleRepositoryI
  , findSampleById
  , insertSample
  , NotFoundError(..)
  )
where

import           RIO
import qualified RIO.Text                      as T

import           Ingestair.Domain.Sample       as DS

type SampleId = Int32

-- Interface for dependency inversion.
-- Following "Clean Architecture" principles, the usecase modules (= controller)
-- must not depend on adapters (persistence).
class SampleRepositoryI sampleRepo where
    findSampleById :: SampleId -> RIO sampleRepo DS.Sample
    insertSample :: DS.Sample -> RIO sampleRepo SampleId

-- Custom Exceptions
newtype NotFoundError = NotFoundError Text
  deriving Typeable
instance Show NotFoundError where
  show (NotFoundError errMsg) = T.unpack $ "Not found: " <> errMsg
instance Display NotFoundError where
  display (NotFoundError errMsg) = display $ "Not found: " <> errMsg
instance Exception NotFoundError
