{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Location.Model
Description : Google Takeout Location to KML Converter
Copyright   : (c) 2024 Frederick Price
License     : BSD-3-Clause
Maintainer  : fprice@pricemail.ca
Stability   : experimental
Portability : POSIX

Command line utility and library to convert Google Takeout Location data to KML format 
-}
module Data.Location.Model (LocationRecord (..), locationRecordParser, LocationRecords (..), locationRecordsParser) where

import Data.Aeson

import Data.Time.Clock
import GHC.Generics
import Prelude

import qualified Data.JsonStream.Parser as J

-- Record that tracks a location IE a Placemark in KML
data LocationRecord = LocationRecord
    { timestamp :: !UTCTime
    , latitudeE7 :: !Int
    , longitudeE7 :: !Int
    , altitude :: !(Maybe Int)
    , accuracy :: !(Maybe Int)
    }
    deriving stock (Generic, Show, Eq, Ord)

-- instance ToJSON LocationRecord
instance FromJSON LocationRecord

-- A list of location records
newtype LocationRecords = LocationRecords
    { locationRecordList :: [LocationRecord]
    }
    deriving stock (Generic, Show, Eq, Ord)

-- instance ToJSON LocationRecords
instance FromJSON LocationRecords

-- The Parser setup
locationRecordParser :: J.Parser LocationRecord
locationRecordParser =
    LocationRecord
        <$> "timestamp" J..: J.value
            <*> "latitudeE7" J..: J.integer
            <*> "longitudeE7" J..: J.integer
            <*> "altitude" J..:? J.integer
            <*> "accuracy" J..:? J.integer

locationRecordsParser :: J.Parser LocationRecord
locationRecordsParser =
    J.objectWithKey "locations" $ J.arrayOf locationRecordParser
