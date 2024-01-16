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
module Data.Location.Model (
-- * Overview
-- $overview

-- * Types
LocationRecord (..),
LocationRecords (..),
) where


import Data.Aeson

import Data.Time.Clock
import GHC.Generics
import Prelude


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

{- $overview
 This module holds LocationRecords
-}


