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
    Latitude,
    Longitude,

    -- * Conversions
    parseLatitude,
    parseLongitude,
    fromE6ScientificLatitude,
    fromE6ScientificLongitude,
    fromE6IntegerLatitude,
    fromE6IntegerLongitude,
    convertLatitudeToBuilder,
    convertLongitudeToBuilder,
) where

import Relude

import Data.Aeson

import Data.Time.Clock

-- import Data.Scientific (Scientific, fromInteger)

import qualified Data.Aeson.Types as AE
import Data.Scientific
import Data.Text.Lazy.Builder as LT
import Data.Text.Lazy.Builder.Scientific

-- Record that tracks a location IE a Placemark in KML
data LocationRecord = LocationRecord
    { timestamp :: !UTCTime
    , latitudeE7 :: !Latitude
    , longitudeE7 :: !Longitude
    , altitude :: !(Maybe Int)
    , accuracy :: !(Maybe Int)
    }
    deriving stock (Generic, Show, Eq, Ord)

-- instance ToJSON LocationRecord
instance FromJSON LocationRecord

newtype Latitude = Latitude
    { latitude :: Scientific
    }
    deriving stock (Generic, Show, Eq, Ord)

-- instance ToSON Latitude
instance FromJSON Latitude where
    parseJSON v =
        case v of
            Number x -> return (fromE6ScientificLatitude x)
            _ -> AE.typeMismatch "Longitude" v
    {-# INLINE parseJSON #-}

newtype Longitude = Longitude
    { longitude :: Scientific
    }
    deriving stock (Generic, Show, Eq, Ord)

instance FromJSON Longitude where
    parseJSON v =
        case v of
            Number x -> return (fromE6ScientificLongitude x)
            _ -> AE.typeMismatch "Longitude" v
    {-# INLINE parseJSON #-}

fromE6ScientificLatitude :: Scientific -> Latitude
fromE6ScientificLatitude x = Latitude (scientific (coefficient x) (base10Exponent x - 7))

fromE6ScientificLongitude :: Scientific -> Longitude
fromE6ScientificLongitude x = Longitude (scientific (coefficient x) (base10Exponent x - 7))

fromE6IntegerLatitude :: Integer -> Latitude
fromE6IntegerLatitude c = Latitude (scientific c (-7))

fromE6IntegerLongitude :: Integer -> Longitude
fromE6IntegerLongitude c = Longitude (scientific c (-7))

parseLongitude :: Int -> Maybe Longitude
parseLongitude x = Just (Longitude (fromInteger (fromIntegral x)))

parseLatitude :: Int -> Maybe Latitude
parseLatitude x = Just (Latitude (fromInteger (fromIntegral x)))

convertLatitudeToBuilder :: Latitude -> Builder
-- convertLatitudeToBuilder x = bformat float (fromIntegral x / 10000000 :: Double)
convertLatitudeToBuilder x = formatScientificBuilder Fixed (Just 7) (latitude x)

convertLongitudeToBuilder :: Longitude -> Builder
-- convertLongitudeToBuilder x = bformat float (fromIntegral x / 10000000 :: Double)
convertLongitudeToBuilder x = formatScientificBuilder Fixed (Just 7) (longitude x)

{- $overview
 This module holds the data definition for LocationRecord which is a container for typical GIS location information.
-}
