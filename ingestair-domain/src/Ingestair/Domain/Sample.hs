{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ingestair.Domain.Sample
  ( MeasurementStatus(..)
  , mkMeasurementStatus
  , mkMeasurementStatusC
  , toText
  , toChar
  , SampleId
  , Timestamp
  , Co2Concentration
  , TemperatureC
  , HumidityPc
  , NodeUID
  , Sample(..)
  , mkSample
  )
where

import           RIO                     hiding ( id )
import qualified RIO.Text                      as T

import qualified Data.Char                     as C
import qualified Data.Scientific               as DS
import qualified Data.UUID                     as UUID

data MeasurementStatus = M | R | E deriving (Eq, Show)

mkMeasurementStatusC :: Char -> Maybe MeasurementStatus
mkMeasurementStatusC s = case C.toUpper s of
  'M' -> Just M
  'R' -> Just R
  'E' -> Just E
  _   -> Nothing

mkMeasurementStatus :: Text -> Maybe MeasurementStatus
mkMeasurementStatus t = T.uncons t >>= mkMeasurementStatusC . fst

toText :: MeasurementStatus -> Text
toText M = "M"
toText R = "R"
toText E = "E"

toChar :: MeasurementStatus -> Char
toChar M = 'M'
toChar R = 'R'
toChar E = 'E'

type SampleId = Int32
type Timestamp = Int32
type Co2Concentration = Int16
type TemperatureC = DS.Scientific
type HumidityPc = Int16
type NodeUID = UUID.UUID

data Sample = Sample
    { id :: !(Maybe SampleId)
    , timestamp_s :: !Timestamp
    , co2_ppm :: !Co2Concentration
    , temperature_celsius :: !(Maybe TemperatureC)
    , rel_humidity_percent :: !(Maybe HumidityPc)
    , measurement_status :: !MeasurementStatus
    , node_id :: !NodeUID
    } deriving (Eq, Show)


-- Construct a valid `Sample` from primitive values
mkSample
  :: Maybe SampleId
  -> Timestamp -- Unix epoch (milliseconds since 1970-01-01T00:00:000Z); > 0.
  -> Co2Concentration -- > 0 PPM
  -> Maybe TemperatureC -- > 0°C
  -> Maybe HumidityPc -- [0%..100%]
  -> Text -- ^ Measurement status. One of `M`, `R`, or `E`.
  -> NodeUID -- ^ Unique Id of the node reporting the measurement. Not Null.
  -> Maybe Sample
mkSample mId timestamp co2 temp hum status node =
  Sample mId
    <$> ensure (> 0) timestamp
    <*> ensure (> 0) co2
    <*> mapM (ensure (> 0)) temp
    <*> mapM (ensure (> 0)) hum
    <*> mkMeasurementStatus status
    <*> ensure (not . UUID.null) node


-- | Ensures that a value satisfies a given predicate.
ensure :: (a -> Bool) -> a -> Maybe a
ensure p v | p v       = Just v
           | otherwise = Nothing
