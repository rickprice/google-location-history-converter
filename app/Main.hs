-- {-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Control.Applicative (many, (<|>))
import qualified Data.Text as T
import qualified Data.JsonStream.Parser as J
import NeatInterpolation (text)
-- import Data.JsonStream.Parser (parseByteString)
import Data.JsonStream.Parser (parseByteString)
-- import Data.ByteString as BS


-- import Data.ByteString.Lazy as BL
-- import Data.ByteString as BS
-- import Data.Text as TS
-- import Data.Text.Lazy as TL
-- import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
-- import Data.ByteString.UTF8 as BSU      -- from utf8-string
import Data.Text.Encoding as TSE
-- import Data.Text.Lazy.Encoding as TLE

main :: IO ()
main = do
    print ( parseByteString resultParser (TSE.encodeUtf8 esTestString))
    print ( parseByteString resultParserIP (TSE.encodeUtf8 inProgressTestString))
    -- print "finished"

-- Original example from json-stream
esTestString::T.Text
esTestString =
    [text|
    {
    "took":42,
      "errors":true,
      "items": [
        {"index": {"_index":"test","_type":"type1","_id":"1","status":400,"error":"Some random error 1"}},
        {"index": {"_index":"test","_type":"type1","_id":"2","status":400,"error":"Some random error 2"}},
        {"index": {"_index":"test","_type":"type1","_id":"3","status":400,"error":"Some random error 3"}},
        {"index":{"_index":"test","_type":"type1","_id":"4","_version":2,"status":200}}
        {"index": {"_index":"test","_type":"type1","_id":"5","status":400,"error":"Some random error 4"}},
        ]
      }
    |]

-- | Result of bulk operation
resultParser :: J.Parser [(T.Text, T.Text)]
resultParser = ([] <$ J.filterI not ("errors" J..: J.bool))
              <|> many ("items" J..: J.arrayOf bulkItemError)

bulkItemError :: J.Parser (T.Text, T.Text)
bulkItemError = J.objectWithKey "index" $
    (,) <$> "_id"   J..: J.string
        <*> "error" J..: J.string
        <*  J.filterI statusError ("status" J..: J.integer)
  where
    statusError s = s < 200 || s > (299 :: Int)



-- In progress changes for Google Location Data
inProgressTestString::T.Text
inProgressTestString =
    [text|
    {
    "took":42,
      "errors":true,
      "items": [
        {"_index":"test","_type":"type1","_id":"1","status":400,"error":"Some random error 1"},
        {"_index":"test","_type":"type1","_id":"2","status":400,"error":"Some random error 2"},
        {"_index":"test","_type":"type1","_id":"3","status":400,"error":"Some random error 3"},
        {"_index":"test","_type":"type1","_id":"4","_version":2,"status":200}
        {"_index":"test","_type":"type1","_id":"5","status":400,"error":"Some random error 4"},
        ]
      }
    |]

-- | Result of bulk operation
resultParserIP :: J.Parser [(T.Text, T.Text, T.Text)]
resultParserIP = ([] <$ J.filterI not ("errors" J..: J.bool))
              <|> many ("items" J..: J.arrayOf bulkItemErrorIP)

bulkItemErrorIP :: J.Parser (T.Text, T.Text, T.Text)
bulkItemErrorIP = J.objectOf $
    (,,) <$> "_id"   J..: J.string
        <*> "error" J..: J.string
        <*> "_type" J..: J.string
        -- <*  J.filterI statusError ("status" J..: J.integer)
  where
    statusError s = s < 200 || s > (299 :: Int)















-- | Result of bulk operation
locationsParser :: J.Parser [(T.Text, T.Text)]
locationsParser = ([] <$ J.filterI not ("errors" J..: J.bool))
              <|> many ("items" J..: J.arrayOf location)

location :: J.Parser (T.Text, T.Text)
location = J.objectWithKey "index" $
    (,) <$> "_id"   J..: J.string
        <*> "error" J..: J.string
        <*  J.filterI statusError ("status" J..: J.integer)
  where
    statusError s = s < 200 || s > (299 :: Int)

jsonLocationsText::T.Text
jsonLocationsText =
    [text|
    {
    "locations": [{
        "latitudeE7": 447405071,
        "longitudeE7": -798735599,
        "accuracy": 13,
        "altitude": 183,
        "verticalAccuracy": 1,
        "activity": [{
        "activity": [{
            "type": "UNKNOWN",
            "confidence": 41
        }, {
            "type": "IN_VEHICLE",
            "confidence": 10
        }, {
            "type": "ON_BICYCLE",
            "confidence": 10
        }, {
            "type": "ON_FOOT",
            "confidence": 10
        }, {
            "type": "STILL",
            "confidence": 10
        }, {
            "type": "WALKING",
            "confidence": 10
        }, {
            "type": "RUNNING",
            "confidence": 10
        }, {
            "type": "IN_ROAD_VEHICLE",
            "confidence": 10
        }, {
            "type": "IN_RAIL_VEHICLE",
            "confidence": 10
        }],
        "timestamp": "2023-11-13T02:04:08.588Z"
        }, {
        "activity": [{
            "type": "STILL",
            "confidence": 96
        }, {
            "type": "ON_FOOT",
            "confidence": 2
        }, {
            "type": "WALKING",
            "confidence": 2
        }, {
            "type": "IN_VEHICLE",
            "confidence": 1
        }, {
            "type": "UNKNOWN",
            "confidence": 1
        }, {
            "type": "IN_ROAD_VEHICLE",
            "confidence": 1
        }, {
            "type": "IN_RAIL_VEHICLE",
            "confidence": 1
        }],
        "timestamp": "2023-11-13T02:05:09.509Z"
        }, {
        "activity": [{
            "type": "STILL",
            "confidence": 100
        }],
        "timestamp": "2023-11-13T02:06:09.664Z"
        }, {
        "activity": [{
            "type": "STILL",
            "confidence": 100
        }],
        "timestamp": "2023-11-13T02:07:10.477Z"
        }, {
        "activity": [{
            "type": "STILL",
            "confidence": 100
        }],
        "timestamp": "2023-11-13T02:08:10.571Z"
        }, {
        "activity": [{
            "type": "STILL",
            "confidence": 100
        }],
        "timestamp": "2023-11-13T02:09:10.730Z"
        }, {
        "activity": [{
            "type": "STILL",
            "confidence": 100
        }],
        "timestamp": "2023-11-13T02:10:10.850Z"
        }, {
        "activity": [{
            "type": "STILL",
            "confidence": 100
        }],
        "timestamp": "2023-11-13T02:11:11.439Z"
        }, {
        "activity": [{
            "type": "STILL",
            "confidence": 100
        }],
        "timestamp": "2023-11-13T02:12:11.531Z"
        }],
        "source": "WIFI",
        "deviceTag": 623593388,
        "platformType": "ANDROID",
        "activeWifiScan": {
        "accessPoints": [{
            "mac": "82179071244990",
            "strength": -57,
            "frequencyMhz": 2412
        }, {
            "mac": "82179071244985",
            "strength": -57,
            "frequencyMhz": 2412
        }, {
            "mac": "82179071244987",
            "strength": -57,
            "frequencyMhz": 2412
        }, {
            "mac": "82179071244991",
            "strength": -57,
            "frequencyMhz": 2412
        }, {
            "mac": "185122854537800",
            "strength": -64,
            "frequencyMhz": 2457
        }, {
            "mac": "64561708141601",
            "strength": -66,
            "frequencyMhz": 2412
        }, {
            "mac": "99746080230433",
            "strength": -66,
            "frequencyMhz": 2412
        }, {
            "mac": "185122854537804",
            "strength": -72,
            "isConnected": true,
            "frequencyMhz": 5745
        }, {
            "mac": "253712216230437",
            "strength": -76,
            "frequencyMhz": 2412
        }, {
            "mac": "268886088754700",
            "strength": -76,
            "frequencyMhz": 2437
        }, {
            "mac": "101694050488993",
            "strength": -77,
            "frequencyMhz": 2437
        }, {
            "mac": "64561708141602",
            "strength": -78,
            "frequencyMhz": 5785
        }, {
            "mac": "101694050488987",
            "strength": -78,
            "frequencyMhz": 2437
        }, {
            "mac": "101694050488988",
            "strength": -78,
            "frequencyMhz": 2437
        }, {
            "mac": "101694050488992",
            "strength": -78,
            "frequencyMhz": 2437
        }, {
            "mac": "203019708105986",
            "strength": -81,
            "frequencyMhz": 2412
        }, {
            "mac": "239309056430",
            "strength": -82,
            "frequencyMhz": 2437
        }, {
            "mac": "11715607655038",
            "strength": -82,
            "frequencyMhz": 2462
        }, {
            "mac": "11715607655035",
            "strength": -83,
            "frequencyMhz": 2462
        }, {
            "mac": "11715607655036",
            "strength": -83,
            "frequencyMhz": 2462
        }, {
            "mac": "165589415991794",
            "strength": -84,
            "frequencyMhz": 2452
        }, {
            "mac": "158992346225138",
            "strength": -85,
            "frequencyMhz": 2452
        }, {
            "mac": "275483158521357",
            "strength": -88,
            "frequencyMhz": 5745
        }, {
            "mac": "233569152951437",
            "strength": -92,
            "frequencyMhz": 2437
        }, {
            "mac": "158992346225214",
            "strength": -92,
            "frequencyMhz": 2452
        }, {
            "mac": "235761187381727",
            "strength": -92,
            "frequencyMhz": 2462
        }, {
            "mac": "64561710254137",
            "strength": -92,
            "frequencyMhz": 2462
        }, {
            "mac": "165589415991870",
            "strength": -93,
            "frequencyMhz": 2452
        }, {
            "mac": "218169001337311",
            "strength": -93,
            "frequencyMhz": 2462
        }, {
            "mac": "233562156108681",
            "strength": -95,
            "frequencyMhz": 2462
        }, {
            "mac": "226965086342025",
            "strength": -95,
            "frequencyMhz": 2462
        }]
        },
        "osLevel": 34,
        "serverTimestamp": "2023-11-13T02:12:22.763Z",
        "deviceTimestamp": "2023-11-13T02:12:21.707Z",
        "batteryCharging": false,
        "formFactor": "PHONE",
        "timestamp": "2023-11-13T02:12:21.458Z"
    }]
    }
    |]

