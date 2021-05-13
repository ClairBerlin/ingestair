{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Abstract collection-like interface to the underlying persistence layer.
module Ingestair.Persistence.SampleRepository
  ( findSampleById
  , insertSample
  )
where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.List.Partial              as PL

import           Opaleye                        ( (.===) )
import qualified Opaleye                       as OE
import qualified Opaleye.Internal.PGTypes      as IPT
import qualified Opaleye.Internal.HaskellDB.PrimQuery
                                               as HPQ
import qualified Data.Profunctor.Product.Default
                                               as PPD


import qualified Ingestair.Domain.Sample       as DS
import qualified Ingestair.Controller.SampleRepositoryI
                                               as SRI
import qualified Ingestair.Persistence.DbConfiguration
                                               as DBC
import qualified Ingestair.Persistence.SamplesTable
                                               as ST
import qualified Ingestair.Persistence.PersistenceUtils
                                               as PU


-- Fix missing SqlInt2 ToFields instance
instance PPD.Default OE.ToFields Int16 (OE.Column OE.SqlInt2) where
  def = OE.toToFields $ sqlInt2 . fromIntegral

sqlInt2 :: Int -> OE.Field OE.SqlInt2
sqlInt2 = pgInt2

pgInt2 :: Int -> OE.Column OE.PGInt2
pgInt2 = IPT.literalColumn . HPQ.IntegerLit . fromIntegral

-- Fix missing Int16 DefaultFromField instance
instance OE.DefaultFromField OE.SqlInt2 Int16 where
  defaultFromField = OE.fromPGSFromField


findSampleById
  :: (HasLogFunc env, DBC.HasDbConnPool env)
  => SRI.SampleId
  -> RIO env DS.Sample
findSampleById sId = do
  logDebug ("Looking up sample with ID " <> display sId <> " in the database.")
  connPool    <- view DBC.connPoolL
  maybeSample <- liftIO $ readSample connPool sId
  sample      <- maybeSample
    `PU.orThrow` SRI.NotFoundError ("No sample with Id " <> tshow sId <> ".")
  ST.toDomain sample

-- readAllSamples :: (DBC.HasDbConnPool env) => RIO env (Either Text [DS.Sample])
-- readAllSamples = do
--   connPool <- view DBC.connPoolL
--   liftIO $ findAllSamples connPool


insertSample
  :: (HasLogFunc env, DBC.HasDbConnPool env)
  => DS.Sample
  -> RIO env SRI.SampleId
insertSample sample = do
  logDebug
    (  "Persisting a sample from node "
    <> displayShow (DS.node_id sample)
    <> " with time stamp "
    <> display (DS.timestamp_s sample)
    )
  connPool <- view DBC.connPoolL
  let sampleEntity        = ST.toPersistenceEntity sample
      insertSpecification = OE.Insert
        { OE.iTable      = ST.sampleTable
        , OE.iRows       = [OE.toFields sampleEntity] -- Insert a single sample only.
        , OE.iReturning  = OE.rReturning ST.getPkField
        , OE.iOnConflict = Nothing
        }
  pks <- liftIO $ createSample connPool insertSpecification
  return $ ST.getPk (PL.head pks) -- Safe, because we inserted a single sample.


--------------------
-- Run DB Access
--------------------
-- Functions in the IO Monad that perform the actual database access, given a
-- connection string. These functions use Opaleye primitives that perform the
-- mapping between Haskell records and Opaleye PostgreSQL records.

-- | Find all samples stored in the DB and return them.
-- Naming convention: DB retrievals are called "find".
findAllSamples :: DBC.ConnPool -> IO [ST.Sample]
findAllSamples connPool = PU.withPostgreSQLPool connPool
  $ \conn -> OE.runSelect conn ST.selectSampleRelVar

-- | Find the sample with given sample Id.
readSample :: DBC.ConnPool -> SRI.SampleId -> IO (Maybe ST.Sample)
readSample connPool sId = PU.withPostgreSQLPool connPool $ \conn -> do
  result <- OE.runSelect conn (restrictBySampleId $ ST.mkPkField sId)
  return $ L.headMaybe result

-- -- | Find the sample with given sample Id.
-- findSample :: DBC.ConnPool -> SRI.SampleId -> IO (Maybe ST.Sample)
-- findSample connPool sId = PU.withPostgreSQLPool connPool $ \conn -> do
--   result <- OE.runSelect
--     conn
--     (sampleByIdQ <<< arr (const $ ST.SampleId (OE.sqlInt4 (fromIntegral sId)))) -- Convert a parameter into an arrow via the const function.
--   return $ L.headMaybe result

createSample :: DBC.ConnPool -> OE.Insert [ST.Pk] -> IO [ST.Pk]
createSample connPool insertSpecification = PU.withPostgreSQLPool connPool
  $ \conn -> OE.runInsert_ conn insertSpecification


--------------------
-- Compund Queries
--------------------
-- The queries below are written in the typesafe Opaleye query DSL. Opaleye
-- translates them into actual SQL statements that can be executed against the
-- DBMS. The query statements specified below are similar to prepared
-- statements; they need to be executed separately.
-- Queries return Opaleye PostgreSQL "Read" records.

restrictBySampleId :: ST.PkField -> OE.Select ST.SampleR
restrictBySampleId sId = do
  row <- ST.selectSampleRelVar
  OE.viaLateral OE.restrict $ ST.getPkField row .=== sId
  pure row
