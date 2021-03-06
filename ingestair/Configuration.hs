{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Configuration
  ( EnvironmentSettings(..)
  , defaultEnvironmentSettings
  , configureLogging
  , HasEnvironment
  , environmentL
  , Env(..)
  )
where

import           RIO
import qualified RIO.Text                      as T

import qualified Network.Wai.Handler.Warp      as Warp
import qualified System.Envy                   as EV

import qualified Ingestair.Usecases.SampleRepositoryI
                                               as SRI
import qualified Ingestair.Persistence.Api     as DB

-- Runtime system environment variables used for configuring the application.
data EnvironmentSettings = EnvironmentSettings
  { serverPort :: !Warp.Port -- SERVER_PORT
  , logLevel :: !Text -- LOG_LEVEL
  , logVerbose :: Bool -- LOG_VERBOSE
  , logUseTime :: Bool -- LOG_USE_TIME
  , logUseLoc :: Bool -- LOG_USE_LOC
  , logUseColor :: Bool -- LOG_USE_COLOR
  , sqlHost :: !Text -- SQL_HOST
  , sqlPort :: !Word16 -- SQL_PORT
  , sqlDatabase :: !Text -- SQL_DATABASE
  , sqlUser :: !Text -- SQL_USER
  , sqlPassword :: !Text -- SQL_PASSWORD
  } deriving (Eq, Show, Generic)

defaultEnvironmentSettings :: EnvironmentSettings
defaultEnvironmentSettings = EnvironmentSettings
  { serverPort  = 8081
  , logLevel    = "WARN"
  , logVerbose  = False
  , logUseTime  = True
  , logUseLoc   = False
  , logUseColor = True
  , sqlHost     = "localhost"
  , sqlPort     = 5432
  , sqlDatabase = "managairdb_dev"
  , sqlUser     = "managair_dev"
  , sqlPassword = "postgres"
  }

readLogLevel :: Text -> Maybe LogLevel
readLogLevel lvl = case (T.toUpper . T.strip) lvl of
  "DEBUG" -> Just LevelDebug
  "INFO"  -> Just LevelInfo
  "WARN"  -> Just LevelWarn
  "ERROR" -> Just LevelError
  _       -> Nothing


configureLogging :: EnvironmentSettings -> LogOptions -> LogOptions
configureLogging eEnv logOptions' =
  let maybeLogLevel = readLogLevel (logLevel eEnv)
      level         = fromMaybe LevelWarn maybeLogLevel
  in  setLogMinLevel level
        $ setLogVerboseFormat (logVerbose eEnv)
        $ setLogUseTime (logUseTime eEnv)
        $ setLogUseLoc (logUseLoc eEnv)
        $ setLogUseColor (logUseColor eEnv) logOptions'

-- Instance for automatically retrieving environment variables.
instance EV.FromEnv EnvironmentSettings

-- Accessor interface for the environment settings.
class HasEnvironment env where
  environmentL :: Lens' env EnvironmentSettings

instance HasEnvironment EnvironmentSettings where
  environmentL = id


-- Configuration environment to be threaded through the entire application.
data Env = Env
  { logger :: !LogFunc
  , environment :: !EnvironmentSettings
  , dBConfig :: !DB.DbConfig
  }

-- Accessors for individual elements of the enviroment.
-- See the "Lenses" section here: https://www.fpcomplete.com/haskell/library/rio/
instance HasLogFunc Env where
  logFuncL = lens logger (\x y -> x { logger = y })

instance HasEnvironment Env where
  environmentL = lens environment (\x y -> x { environment = y })

instance DB.HasDbConfig Env where
  dBConfigL = lens dBConfig (\x y -> x { dBConfig = y })

instance DB.HasDbConnection Env where
  connInfoL = DB.dBConfigL . DB.connInfoL

instance DB.HasDbConnPool Env where
  connPoolL = DB.dBConfigL . DB.connPoolL

-- Implement the SampleRepositoryI interface to wire up the inverted dependencies.
instance SRI.SampleRepositoryI Env where
  findSampleById = DB.findSampleById
  insertSample   = DB.insertSample
