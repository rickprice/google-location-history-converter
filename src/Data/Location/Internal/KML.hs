{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module Data.Location.Internal.KML (xmlKMLHeader, xmlKMLFooter, toPlacemarkDataTag, convertLocationToBuilder, wrapWithDataTag, toExtendedDataTag) where

import Data.Location.Model

import Data.Time.Format.ISO8601
import Prelude

import Data.ByteString.Builder

-- The KML Header
xmlKMLHeader :: Builder
xmlKMLHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>\n"

-- The KML Footer
xmlKMLFooter :: Builder
xmlKMLFooter = "</Document></kml>"

toExtendedDataTag :: LocationRecord -> Builder
toExtendedDataTag loc = "<ExtendedData>" <> tagContents <> "</ExtendedData>"
  where
    tagContents = mconcat [wrapWithDataTag "accuracy" (accuracy loc), wrapWithDataTag "altitude" (altitude loc)]

wrapWithDataTag :: Builder -> Maybe Int -> Builder
wrapWithDataTag name (Just x) = "<Data name=\"" <> name <> "\"><value>" <> intDec x <> "</value></Data>"
wrapWithDataTag _ Nothing = mempty

toPlacemarkDataTag :: LocationRecord -> Builder
toPlacemarkDataTag x =
    "<Placemark>"
        <> "<TimeStamp><when>"
        <> string7 (iso8601Show (timestamp x))
        <> "</when></TimeStamp>"
        <> toExtendedDataTag x
        <> "<Point><coordinates>"
        <> convertLocationToBuilder (longitudeE7 x)
        <> ","
        <> convertLocationToBuilder (latitudeE7 x)
        <> "</coordinates></Point>"
        <> "</Placemark>\n"

convertLocationToBuilder :: Int -> Builder
convertLocationToBuilder x = doubleDec (fromIntegral x / 10000000 :: Double)
