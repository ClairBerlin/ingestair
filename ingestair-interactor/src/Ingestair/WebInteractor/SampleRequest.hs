{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Ingestair.WebInteractor.SampleRequest
  ( SampleRequestData(..)
  , SampleRequestDocument(..)
  )
where

import           RIO

import qualified Data.Aeson                    as J

import           Ingestair.WebInteractor.SampleResource
                                               as SR

-- For parsing incoming JSON:API requests.
data SampleRequestData = SampleRequestData
  { data_type :: !Text
  , data_attributes :: !SR.SampleResource
  } deriving (Eq, Show, Generic)

instance J.FromJSON SampleRequestData where
  parseJSON = J.genericParseJSON J.defaultOptions
    { J.fieldLabelModifier = drop 5
    , J.omitNothingFields  = True
    }

newtype SampleRequestDocument = SampleRequestDocument { doc_data :: SampleRequestData } deriving (Eq, Show, Generic)

instance J.FromJSON SampleRequestDocument where
  parseJSON = J.genericParseJSON J.defaultOptions
    { J.fieldLabelModifier = drop 4
    , J.omitNothingFields  = True
    }
