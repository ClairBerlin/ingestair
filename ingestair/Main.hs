{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import           RIO

import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Middleware.Gzip   as GZ
import qualified Network.Wai.Middleware.RequestLogger
                                               as RL
import qualified System.Envy                   as EV

import qualified Ingestair.Controller.Server   as S
import qualified Ingestair.Persistence.Api     as DB
import qualified Logging                       as L
import qualified Configuration                 as Conf


main :: IO ()
main = do
  -- Read configuration from environment variables.
  eEnv <- EV.decodeWithDefaults Conf.defaultEnvironmentSettings
  -- Set up the application configuration that is threaded throughout the entire 
  -- application using the ReaderT design pattern by means of the RIO monad.
  let dBConfig = DB.initDb (Conf.sqlHost eEnv)
                           (Conf.sqlPort eEnv)
                           (Conf.sqlDatabase eEnv)
                           (Conf.sqlUser eEnv)
                           (Conf.sqlPassword eEnv)
      env = Conf.Env { Conf.logger      = L.myLogFunc
                     , Conf.environment = eEnv
                     , Conf.dBConfig    = dBConfig
                     }
      serverPort = Conf.serverPort eEnv
  -- Apply middleware and start the web server:
  -- > Support for gzip compression.
  -- > Log requests to stdout.
  Warp.run serverPort (GZ.gzip GZ.def (RL.logStdout $ S.serverApp env))