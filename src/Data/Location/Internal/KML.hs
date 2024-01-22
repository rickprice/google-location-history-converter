{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      : Data.Location.Internal.KML
Description : Google Takeout Location to KML Converter
Copyright   : (c) 2024 Frederick Price
License     : BSD-3-Clause
Maintainer  : fprice@pricemail.ca
Stability   : experimental
Portability : POSIX

Command line utility and library to convert Google Takeout Location data to KML format
-}
module Data.Location.Internal.KML (xmlKMLHeader, xmlKMLFooter, toPlacemarkDataTag, wrapMaybeTag, wrapMaybeDataTag, wrapExtendedValues) where

import Relude

import Data.Location.Model

import Data.Time.Format.ISO8601

import Data.Text.Lazy.Builder

import Formatting

-- The KML Header
xmlKMLHeader :: Builder
xmlKMLHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>\n"

-- The KML Footer
xmlKMLFooter :: Builder
xmlKMLFooter = "</Document></kml>"

toPlacemarkDataTag :: LocationRecord -> Builder
toPlacemarkDataTag x =
    "<Placemark>"
        <> "<TimeStamp><when>"
        <> bformat string (iso8601Show (timestamp x))
        <> "</when></TimeStamp>"
        <> fromMaybe "" (wrapExtendedValues x)
        <> "<Point><coordinates>"
        <> convertLatitudeToBuilder (latitudeE7 x)
        <> ","
        <> convertLongitudeToBuilder (longitudeE7 x)
        <> "</coordinates></Point>"
        <> "</Placemark>\n"

wrapMaybeTag :: Builder -> Maybe Builder -> Maybe Builder
wrapMaybeTag tagName (Just v) = Just $ "<" <> tagName <> ">" <> v <> "</" <> tagName <> ">"
wrapMaybeTag _ Nothing = Nothing

wrapExtendedDataTag :: Maybe Builder -> Maybe Builder
wrapExtendedDataTag = wrapMaybeTag "ExtendedData"

wrapMaybeDataTag :: Builder -> Maybe Int -> Maybe Builder
wrapMaybeDataTag tagClass (Just v) = Just $ "<Data name=\"" <> tagClass <> "\"><value>" <> bformat int v <> "</value></Data>"
wrapMaybeDataTag _ Nothing = Nothing

wrapMaybeAccuracy :: LocationRecord -> Maybe Builder
wrapMaybeAccuracy lr = wrapMaybeDataTag "accuracy" (accuracy lr)

wrapMaybeAltitude :: LocationRecord -> Maybe Builder
wrapMaybeAltitude lr = wrapMaybeDataTag "altitude" (altitude lr)

wrapExtendedValues :: LocationRecord -> Maybe Builder
wrapExtendedValues lr = wrapExtendedDataTag $ wrapMaybeAccuracy lr <> wrapMaybeAltitude lr
