{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ingestair.Persistence.SamplesTable
  ( PkT(..)
  , PkField
  , mkPkField
  , Pk
  , SampleR
  , Sample
  , SampleI
  , getPkField
  , sampleTable
  , selectSampleRelVar
  , LoadTypeError()
  , toDomain
  , toPersistenceEntity
  )
where

import           RIO
import qualified RIO.Text                      as T

import qualified Data.Profunctor.Product        ( )
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance )
import qualified Opaleye                       as OE
import qualified Ingestair.Domain.Sample       as DS
import qualified Ingestair.Persistence.PersistenceUtils
                                               as PU


------------------------------
-- Dedicated Primary Key Type
-----------------------------

type HsPk = Int32
type SqlPk = OE.SqlInt4

newtype PkT a = Pk {getPk :: a}
  deriving (Eq, Show, Display)

$(makeAdaptorAndInstance "pPk" ''PkT)

type PkField = PkT (OE.Field SqlPk)

mkPkField :: HsPk -> PkField
mkPkField pk = Pk (OE.sqlInt4 $ fromIntegral pk)

type OptionalPkField = PkT (Maybe (OE.Field SqlPk))

type Pk = PkT HsPk


--------------------
-- Table Setup
--------------------

schemaName :: String
schemaName = "public"

sampleTableName :: String
sampleTableName = "core_sample"

-- | Polymorphic type for the "core_sample" table. This type describes the "shape" of
-- a record, but not the type of individual record fields. This allows Opaleye
-- to map Haskell types to PostgreSQL types by specializing the type parameters.
data SampleT key timestamp_s co2_ppm temperature_celsius rel_humidity_percent measurement_status node_id
  = Sample
      { sample_key :: !key
      ,  sample_timestamp_s :: !timestamp_s
      ,  sample_co2_ppm :: !co2_ppm
      ,  sample_temperature_celsius :: !temperature_celsius
      ,  sample_rel_humidity_percent :: !rel_humidity_percent
      ,  sample_measurement_status :: !measurement_status
      ,  sample_node_id :: !node_id
      }
  deriving (Show)

-- | Record that Opaleye uses to write to the "core_sample" table. This record uses
-- native PostgreSQL types. Read- and write-records are different to allow
-- PostgreSQL DB to perform actions upon writing certain fields, like
-- auto-generate a key, or set the insertion date.
type SampleW
  = SampleT
      OptionalPkField -- autogenerated key
      (OE.Field OE.SqlInt4) -- timestamp_s
      (OE.Field OE.SqlInt2) -- co2_ppm
      (OE.FieldNullable OE.SqlNumeric) -- optional temperature_celsius
      (OE.FieldNullable OE.SqlInt2) -- optional rel_humidity_percent
      (OE.Field OE.SqlText) -- measurement_status
      (OE.Field OE.SqlUuid) -- node_id

-- | Record that Opaleye reads from the "user" table. This record uses nativ
-- PostreSQL types.
type SampleR
  = SampleT
      PkField -- key
      (OE.Field OE.SqlInt4) -- timestamp_s
      (OE.Field OE.SqlInt2) -- co2_ppm
      (OE.FieldNullable OE.SqlNumeric) -- optional temperature_celsius
      (OE.FieldNullable OE.SqlInt2) -- optional rel_humidity_percent
      (OE.Field OE.SqlText) -- measurement_status
      (OE.Field OE.SqlUuid) -- password hash

-- | Typesafe Haskell record to interface with the application. Under the hood,
-- Opaleye converts between this application record and the above PostgreSQL
-- read and write records.
type Sample
  = SampleT
      Pk -- key
      DS.Timestamp -- timestamp_s
      DS.Co2Concentration -- co2_ppm
      (Maybe DS.TemperatureC) -- temperature_celsius
      (Maybe DS.HumidityPc) -- rel_humidity_percenmt
      Text -- measurement_status
      DS.NodeUID -- node_id

type SampleI
  = SampleT
      (PkT (Maybe HsPk)) -- autogenerated primary key
      DS.Timestamp -- timestamp_s
      DS.Co2Concentration -- co2_ppm
      (Maybe DS.TemperatureC) -- temperature_celsius
      (Maybe DS.HumidityPc) -- rel_humidity_percent
      Text -- measurement_status
      DS.NodeUID -- node_id

instance Display Sample where
  display = displayShow

-- | Template Haskell helper to create the mapping function between PostgreSQL
-- records and the Haskell record used below.
$(makeAdaptorAndInstance "pSample" ''SampleT)

-- | The actual mapping setup tells Opaleye exactly how to map between the
-- PostgreSQL records and the Haskell record. For each record, the function
-- specifies the name of the table column and the constraints.
sampleTable :: OE.Table SampleW SampleR
sampleTable = OE.tableWithSchema
  schemaName
  sampleTableName
  (pSample Sample
    { sample_key                  = pPk (Pk (OE.tableField "id"))
    , sample_timestamp_s          = OE.tableField "timestamp_s"
    , sample_co2_ppm              = OE.tableField "co2_ppm"
    , sample_temperature_celsius  = OE.tableField "temperature_celsius"
    , sample_rel_humidity_percent = OE.tableField "rel_humidity_percent"
    , sample_measurement_status   = OE.tableField "measurement_status"
    , sample_node_id              = OE.tableField "node_id"
    }
  )

getPkField :: SampleR -> PkField
getPkField = sample_key


--------------------
-- Basic Query
--------------------

-- | Select the entire Sample table.
selectSampleRelVar :: OE.Select SampleR
selectSampleRelVar = OE.selectTable sampleTable

--------------------
-- Conversions
--------------------
newtype LoadTypeError = LoadTypeError Text
    deriving Typeable
instance Show LoadTypeError where
  show (LoadTypeError errMsg) =
    T.unpack $ "Type error loading data: " <> errMsg
instance Display LoadTypeError where
  display (LoadTypeError errMsg) =
    display $ "Type error loading data: " <> errMsg
instance Exception LoadTypeError


toDomain :: MonadThrow m => Sample -> m DS.Sample
toDomain st = maybeSample `PU.orThrow` LoadTypeError
  (  "The sample stored at database ID "
  <> tshow (sample_key st)
  <> " is invalid."
  )
 where
  maybeSample = DS.mkSample (sample_timestamp_s st)
                            (sample_co2_ppm st)
                            (sample_temperature_celsius st)
                            (sample_rel_humidity_percent st)
                            (sample_measurement_status st)
                            (sample_node_id st)


toPersistenceEntity :: DS.Sample -> SampleI
toPersistenceEntity sample = Sample
  { sample_key                  = Pk (Nothing :: Maybe HsPk) -- DBMS-generated
  , sample_timestamp_s          = DS.timestamp_s sample
  , sample_co2_ppm              = DS.co2_ppm sample
  , sample_temperature_celsius  = DS.temperature_celsius sample
  , sample_rel_humidity_percent = DS.rel_humidity_percent sample
  , sample_measurement_status   = DS.toText $ DS.measurement_status sample
  , sample_node_id              = DS.node_id sample
  }
