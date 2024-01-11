{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module Data.Location.Internal.KML (convertLocation, xmlGISHeader, xmlGISFooter, toGISBody) where
import Data.Location.Model

import Data.Time.Format.ISO8601
import Prelude

-- Convert a location to a string value for KML
convertLocation :: Int -> String
convertLocation x = reverse (start ++ "." ++ end)
  where
    (start, end) = splitAt 7 (reverse $ show x)


-- The KML Header
xmlGISHeader :: String
xmlGISHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>"

toData :: String -> Maybe Int -> String
toData _ Nothing = ""
toData name (Just x) = "<Data name=\"" ++ name ++ "\"><value>" ++ show x ++ "</value></Data>"

extendedData' :: LocationRecord -> String
extendedData' x = mconcat [toData "accuracy" (accuracy x), toData "altitude" (altitude x)]

extendedData :: String -> String
extendedData x = if null x then "" else "<ExtendedData>" ++ x ++ "</ExtendedData>"

toGISBody :: LocationRecord -> String
toGISBody x =
    "<Placemark>"
        ++ "<TimeStamp><when>"
        ++ iso8601Show (timestamp x)
        ++ "</when></TimeStamp>"
        ++ extendedData (extendedData' x)
        ++ "<Point><coordinates>"
        ++ convertLocation (longitudeE7 x)
        ++ ","
        ++ convertLocation (latitudeE7 x)
        ++ "</coordinates></Point>"
        ++ "</Placemark>"

xmlGISFooter :: String
xmlGISFooter = "</Document></kml>"
