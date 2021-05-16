{-# LANGUAGE NoImplicitPrelude #-}

-- Utility functions to simplify using Opaleye and PostfreSQL-simple
module Ingestair.Persistence.PersistenceUtils
  ( withPostgreSQL
  , withPostgreSQLPool
  , orThrow
  )
where

import           RIO
import qualified Database.PostgreSQL.Simple    as PGS
import qualified Data.Pool                     as DPL

import qualified Ingestair.Persistence.DbConfiguration
                                               as DBC


-- | Safely execute a DB action: Properly acquire the DB connection and clean 
-- up the resources afterwards.
withPostgreSQL :: PGS.ConnectInfo -> (PGS.Connection -> IO a) -> IO a
withPostgreSQL connInfo = bracket (PGS.connect connInfo) -- acquire
                                  PGS.close -- release

-- | Safely execute a DB action using a DB connection from a pool.
withPostgreSQLPool :: DBC.ConnPool -> (PGS.Connection -> IO a) -> IO a
withPostgreSQLPool (DBC.ConnPool poolIO) action = do
  pool <- poolIO
  DPL.withResource pool action


-- For simplified error handling.
-- See https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html
orThrow :: (MonadThrow m, Exception e) => Maybe a -> e -> m a
orThrow Nothing    e = throwM e
orThrow (Just val) _ = return val
