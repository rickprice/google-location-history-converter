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
module Data.Location.Internal.KML (xmlKMLHeader, xmlKMLFooter, toPlacemarkDataTag, convertLocationToBuilder, wrapWithDataTag, toExtendedDataTag) where

import Relude

import Data.Location.Model

import Data.Time.Format.ISO8601

import Data.Text.Lazy.Builder

import Formatting

import Data.Maybe (fromJust)

-- The KML Header
xmlKMLHeader :: Builder
xmlKMLHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>\n"

-- The KML Footer
xmlKMLFooter :: Builder
xmlKMLFooter = "</Document></kml>"

extendedDataToProcess :: [(Builder, LocationRecord -> Maybe Int)]
extendedDataToProcess = [("accuracy", accuracy), ("altitude", altitude)]

toExtendedDataTag :: LocationRecord -> Builder
toExtendedDataTag loc = if null extendedDataList then mempty else "<ExtendedData>" <> tagContents <> "</ExtendedData>"
  where
    tagContents = mconcat (fmap wrapWithDataTag extendedDataList)
    extendedDataList = extendedValues loc extendedDataToProcess
    extendedValues loc' = mapMaybe (\(x, y) -> if isJust (y loc') then Just (x, fromJust (y loc')) else Nothing)

wrapWithDataTag :: (Builder, Int) -> Builder
wrapWithDataTag (x, y) = "<Data name=\"" <> x <> "\"><value>" <> bformat int y <> "</value></Data>"

toPlacemarkDataTag :: LocationRecord -> Builder
toPlacemarkDataTag x =
    "<Placemark>"
        <> "<TimeStamp><when>"
        <> bformat string (iso8601Show (timestamp x))
        <> "</when></TimeStamp>"
        <> toExtendedDataTag x
        <> "<Point><coordinates>"
        <> convertLocationToBuilder (longitudeE7 x)
        <> ","
        <> convertLocationToBuilder (latitudeE7 x)
        <> "</coordinates></Point>"
        <> "</Placemark>\n"

convertLocationToBuilder :: Int -> Builder
convertLocationToBuilder x = bformat float (fromIntegral x / 10000000 :: Double)
