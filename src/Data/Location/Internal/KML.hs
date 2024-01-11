{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module Data.Location.Internal.KML (xmlGISHeader, xmlGISFooter, toPlacemarkDataTag, convertLocation, wrapWithDataTag, toExtendedDataTag) where

import Data.Location.Model

import Data.Time.Format.ISO8601
import Prelude

-- The KML Header
xmlGISHeader :: String
xmlGISHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>"

-- The KML Footer
xmlGISFooter :: String
xmlGISFooter = "</Document></kml>"

toExtendedDataTag :: LocationRecord -> String
toExtendedDataTag loc = if null tagContents then "" else "<ExtendedData>" ++ tagContents ++ "</ExtendedData>"
  where
    tagContents = mconcat [wrapWithDataTag "accuracy" (accuracy loc), wrapWithDataTag "altitude" (altitude loc)]

wrapWithDataTag :: String -> Maybe Int -> String
wrapWithDataTag _ Nothing = ""
wrapWithDataTag name (Just x) = "<Data name=\"" ++ name ++ "\"><value>" ++ show x ++ "</value></Data>"

toPlacemarkDataTag :: LocationRecord -> String
toPlacemarkDataTag x =
    "<Placemark>"
        ++ "<TimeStamp><when>"
        ++ iso8601Show (timestamp x)
        ++ "</when></TimeStamp>"
        ++ toExtendedDataTag x
        ++ "<Point><coordinates>"
        ++ convertLocation (longitudeE7 x)
        ++ ","
        ++ convertLocation (latitudeE7 x)
        ++ "</coordinates></Point>"
        ++ "</Placemark>"

-- Convert a location to a string value for KML
convertLocation :: Int -> String
convertLocation x = reverse (start ++ "." ++ end)
  where
    (start, end) = splitAt 7 (reverse $ show x)
