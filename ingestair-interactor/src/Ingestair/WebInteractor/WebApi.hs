{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ingestair.WebInteractor.WebApi
  ( ServerApi
  , apiProxy
  )
where

import           RIO

import           Servant                        ( (:>)
                                                , (:<|>)
                                                )
import qualified Servant                       as SV

import qualified Ingestair.WebInteractor.SampleResource as SR
import qualified Ingestair.WebInteractor.SampleRequest as SQ



type IngestEndpoint
  = "ingest" :> "v1" :> "ingest" :> (
          SV.ReqBody '[SV.JSON] SQ.SampleRequestDocument :> SV.PostCreated '[SV.JSON] SR.SampleId
    :<|>  SV.Capture "id" SR.SampleId :> SV.Get '[SV.JSON] SR.SampleDocument
  )

type ServerApi = IngestEndpoint

apiProxy :: Proxy ServerApi
apiProxy = Proxy
