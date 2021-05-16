{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ingestair.WebInteractor.Server
  ( serverApp
  )
where

import           RIO

import           Control.Monad.Except           ( ExceptT(..) )

import           Servant                        ( (:<|>)(..) )
import qualified Servant                       as SV
import qualified Network.Wai                   as Wai

import qualified Ingestair.WebInteractor.WebApi
                                               as Api
import qualified Ingestair.WebInteractor.IngestEndpoint
                                               as ING
import qualified Ingestair.Usecases.SampleRepositoryI
                                               as SRI


-- Create a Wai application from the hoisted Servant server.
serverApp
  :: (HasLogFunc env, SRI.SampleRepositoryI env) => env -> Wai.Application
serverApp env = SV.serve Api.apiProxy $ hoistRIOServer env


server
  :: (HasLogFunc env, SRI.SampleRepositoryI env)
  => SV.ServerT Api.ServerApi (RIO env)
server = ING.ingestSample :<|> ING.getSample


-- Helper function to hoist our RIO handler monad into a Servant Handler.
hoistRIOServer
  :: (HasLogFunc env, SRI.SampleRepositoryI env)
  => env
  -> SV.Server Api.ServerApi
hoistRIOServer env = SV.hoistServer Api.apiProxy (nt env) server
 where
    -- Natural transformation to map the RIO monad stack to Servant's Handler.
    -- We want to use the RIO monad for our application to run in, instead of Servant's 
    -- regular Handler monad that uses the ExceptT antipattern.
    -- https://harporoeder.com/posts/servant-13-reader-io/
  nt :: env -> RIO env a -> SV.Handler a
  nt e m = SV.Handler $ ExceptT $ try $ runRIO e m
