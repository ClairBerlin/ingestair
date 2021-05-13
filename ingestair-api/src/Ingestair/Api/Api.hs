{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ingestair.Api.Api
  ( SampleRequest(..)
  , SampleId
  , ServerApi
  , apiProxy
  )
where

import           RIO

import qualified Data.Scientific               as DS
import qualified Data.UUID                     as UUID

import           Servant                        ( (:>)
                                                , (:<|>)
                                                )
import qualified Servant                       as SV
import qualified Data.Aeson                    as J
import qualified Ingestair.Domain.Sample       as S


data SampleRequest = SampleRequest
    { timestamp_s :: !Int32
    , co2_ppm :: !Int16
    , temperature_celsius :: !(Maybe DS.Scientific)
    , rel_humidity_percent :: !(Maybe Int16)
    , measurement_status :: !Text
    , node_id :: !UUID.UUID
    } deriving (Eq, Show, Generic)

instance J.FromJSON SampleRequest

type SampleId = Int32


type IngestEndpoint
  = "ingest" :> "v1" :> "ingest" :> (
          SV.ReqBody '[SV.JSON] SampleRequest :> SV.PostCreated '[SV.JSON] SampleId
    :<|>  SV.Capture "id" SampleId :> SV.Get '[SV.JSON] S.Sample
  )

type ServerApi = IngestEndpoint

apiProxy :: Proxy ServerApi
apiProxy = Proxy
