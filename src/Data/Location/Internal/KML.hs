{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module Data.Location.Internal.KML (xmlKMLHeader, xmlKMLFooter, toPlacemarkDataTag, convertLocationToBuilder, wrapWithDataTag, toExtendedDataTag) where

import Data.Location.Model

import Data.Time.Format.ISO8601
import Prelude

import Data.Text.Lazy.Builder

import Formatting

import Data.Maybe

-- The KML Header
xmlKMLHeader :: Builder
xmlKMLHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>\n"

-- The KML Footer
xmlKMLFooter :: Builder
xmlKMLFooter = "</Document></kml>"

-- toExtendedDataTag :: LocationRecord -> String
-- toExtendedDataTag loc = if null tagContents then "" else "<ExtendedData>" ++ tagContents ++ "</ExtendedData>"
--   where
--     tagContents = mconcat [wrapWithDataTag "accuracy" (accuracy loc), wrapWithDataTag "altitude" (altitude loc)]
-- toExtendedDataTag loc = if null (toLazyText tagContents) then mempty else "<ExtendedData>" <> tagContents <> "</ExtendedData>"
--

extendedDataToProcess:: [(Builder,LocationRecord-> Maybe Int)]
extendedDataToProcess = [("accuracy",accuracy),("altitude",altitude)]

extendedValues:: LocationRecord -> [(Builder, LocationRecord -> Maybe Int)] -> [(Builder, Int)]
extendedValues loc xs =  mapMaybe (\(x,y) -> if isJust (y loc) then Just (x, fromJust (y loc)) else Nothing) xs


toExtendedDataTag :: [(Builder, Int)] -> Builder
toExtendedDataTag xs = if null xs then mempty else "<ExtendedData>" <> tagContents <> "</ExtendedData>"
  where
    tagContents = mconcat (fmap  wrapWithDataTag xs)

wrapWithDataTag :: (Builder, Int) -> Builder
wrapWithDataTag (x,y) = "<Data name=\"" <> x <> "\"><value>" <> bformat int y <> "</value></Data>"

toPlacemarkDataTag :: LocationRecord -> Builder
toPlacemarkDataTag x =
    "<Placemark>"
        <> "<TimeStamp><when>"
        <> bformat string (iso8601Show (timestamp x))
        <> "</when></TimeStamp>"
        <> toExtendedDataTag (extendedValues x extendedDataToProcess)
        <> "<Point><coordinates>"
        <> convertLocationToBuilder (longitudeE7 x)
        <> ","
        <> convertLocationToBuilder (latitudeE7 x)
        <> "</coordinates></Point>"
        <> "</Placemark>\n"

convertLocationToBuilder :: Int -> Builder
convertLocationToBuilder x = bformat float (fromIntegral x / 10000000 :: Double)
