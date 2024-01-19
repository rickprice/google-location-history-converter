{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      : Data.Location.Model
Description : Google Takeout Location to KML Converter
Copyright   : (c) 2024 Frederick Price
License     : BSD-3-Clause
Maintainer  : fprice@pricemail.ca
Stability   : experimental
Portability : POSIX

The Rosetta Stone module that defines GIS Location records
-}
module Data.Location.Model (
    -- * Overview
    -- $overview

    -- * Types
    LocationRecord (..),
    LocationRecords (..),
) where

import Relude

import Data.Aeson

import Data.Time.Clock

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
 This module holds the data definition for LocationRecord which is a container for typical GIS location information.
-}
