{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ingestair.WebGateway.WebApi
  ( ServerApi
  , apiProxy
  )
where

import           RIO

import           Servant                        ( (:>)
                                                , (:<|>)
                                                )
import qualified Servant                       as SV

import qualified Ingestair.WebGateway.SampleResource as SR
import qualified Ingestair.WebGateway.SampleRequest as SQ



type IngestEndpoint
  = "ingest" :> "v1" :> "ingest" :> (
          SV.ReqBody '[SV.JSON] SQ.SampleRequestDocument :> SV.PostCreated '[SV.JSON] SR.SampleId
    :<|>  SV.Capture "id" SR.SampleId :> SV.Get '[SV.JSON] SR.SampleDocument
  )

type ServerApi = IngestEndpoint

apiProxy :: Proxy ServerApi
apiProxy = Proxy
