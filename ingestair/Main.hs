{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import           RIO

import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Middleware.Gzip   as GZ
import qualified Network.Wai.Middleware.RequestLogger
                                               as RL
import qualified System.Envy                   as EV

import qualified Ingestair.WebGateway.Server   as S
import qualified Ingestair.Persistence.Api     as DB
import qualified Configuration                 as Conf


main :: IO ()
main = do
  -- Read configuration from environment variables.
  eEnv        <- EV.decodeWithDefaults Conf.defaultEnvironmentSettings
  -- Route logging to stdout
  logOptions' <- logOptionsHandle stdout True
  -- Set up the application configuration that is threaded throughout the entire 
  -- application using the ReaderT design pattern by means of the RIO monad.
  let logOptions = Conf.configureLogging eEnv logOptions'
      dBConfig   = DB.initDb (Conf.sqlHost eEnv)
                             (Conf.sqlPort eEnv)
                             (Conf.sqlDatabase eEnv)
                             (Conf.sqlUser eEnv)
                             (Conf.sqlPassword eEnv)
      serverPort = Conf.serverPort eEnv
  withLogFunc logOptions $ \logFunc -> do
    let env = Conf.Env { Conf.logger      = logFunc -- L.myLogFunc
                       , Conf.environment = eEnv
                       , Conf.dBConfig    = dBConfig
                       }
    -- Apply middleware and start the web server:
    -- > Support for gzip compression.
    -- > Log requests to stdout.
    Warp.run serverPort (GZ.gzip GZ.def (RL.logStdout $ S.serverApp env))
