{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module Model (LocationRecord (..), locationRecordParser, LocationRecords (..), locationRecordsParser, toXMLString) where

import Data.Aeson

import Data.Time.Clock
import Data.Time.Format.ISO8601
import GHC.Generics
import Prelude

import qualified Data.JsonStream.Parser as J

data LocationRecord = LocationRecord
    { timestamp :: !UTCTime
    , longitudeE7 :: !Int
    , latitudeE7 :: !Int
    , altitude :: !Int
    , accuracy :: !Int
    }
    deriving stock (Generic, Show, Eq, Ord)

-- instance ToJSON LocationRecord
instance FromJSON LocationRecord

data LocationRecords = LocationRecords
    { locationRecordList :: [LocationRecord]
    }
    deriving stock (Generic, Show, Eq, Ord)

-- instance ToJSON LocationRecords
instance FromJSON LocationRecords

locationRecordParser :: J.Parser LocationRecord
locationRecordParser =
    LocationRecord
        <$> "timestamp" J..: J.value
            <*> "latitudeE7" J..: J.integer
            <*> "longitudeE7" J..: J.integer
            <*> "altitude" J..: J.integer
            <*> "accuracy" J..: J.integer

locationRecordsParser :: J.Parser LocationRecord
locationRecordsParser =
    J.objectWithKey "locations" $ J.arrayOf locationRecordParser

convertLocation :: Int -> String
convertLocation x = reverse (start ++ "." ++ end)
  where
    (start, end) = splitAt 7 (reverse $ show x)

xmlGISHeader :: String
xmlGISHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>"

toData :: String -> Int -> String
toData name x = "<Data name=\"" ++ name ++ "\"><value>" ++ show x ++ "</value></Data>"

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

toXMLString :: [LocationRecord] -> String
toXMLString x =
    xmlGISHeader
        ++ concatMap toGISBody x
        ++ xmlGISFooter
