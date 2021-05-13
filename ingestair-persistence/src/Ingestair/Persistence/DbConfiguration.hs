{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Ingestair.Persistence.DbConfiguration
  (DbConfig(..)
  , HasDbConfig
  , dBConfigL
  , ConnPool(..)
  , HasDbConnection()
  , HasDbConnPool()
  , connInfoL
  , connPoolL
  , initDb
  )
where

import           RIO

import qualified Database.PostgreSQL.Simple    as PGS
import qualified Data.Pool                     as DPL
import qualified RIO.Text                      as T


-- | Configuration of the PostgreSQL DBMS.
-- So far, we connect to the DB via a simple connection string.
-- TODO: Use connection pool.
data DbConfig = DbConfig
  { connInfo :: !PGS.ConnectInfo
  , connPool :: !ConnPool }

class HasDbConfig config where
  dBConfigL :: Lens' config DbConfig

instance HasDbConfig DbConfig where
  dBConfigL = id


class HasDbConnection dbConfig where
  connInfoL :: Lens' dbConfig PGS.ConnectInfo

instance HasDbConnection PGS.ConnectInfo where
  connInfoL = id

instance HasDbConnection DbConfig where
  connInfoL = lens connInfo (\x y -> x { connInfo = y })


-- | The connection pool.
newtype ConnPool = ConnPool { getConnPool :: IO (DPL.Pool PGS.Connection) }

class HasDbConnPool dbConfig where
  connPoolL :: Lens' dbConfig ConnPool

instance HasDbConnPool ConnPool where
  connPoolL = id

instance HasDbConnPool DbConfig where
  connPoolL = lens connPool (\x y -> x { connPool = y })

-- | Initialize the DB configuration environment that consists of the 
-- connection information and a connection pool.
-- TODO: Remove the connection information once the pool is working.
initDb :: Text -> Word16 -> Text -> Text -> Text -> DbConfig
initDb dBHost dBPort dBname dBUser dBPassword = DbConfig
  { connInfo = connectionInfo
  , connPool = connectionPool
  }
 where
  connectionInfo = PGS.ConnectInfo { PGS.connectHost     = T.unpack dBHost
                                   , PGS.connectPort     = dBPort
                                   , PGS.connectDatabase = T.unpack dBname
                                   , PGS.connectPassword = T.unpack dBPassword
                                   , PGS.connectUser     = T.unpack dBUser
                                   }
  connectionPool = ConnPool $ DPL.createPool (PGS.connect connectionInfo)
                                             PGS.close
                                             1 -- stripes
                                             60 -- keep unused connections open for 60s
                                             10 -- max. 10 connections open per stripe

