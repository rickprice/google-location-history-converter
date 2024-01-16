-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : Data.Location.GoogleLocationSpec
Description : Google Takeout Location to KML Converter
Copyright   : (c) 2024 Frederick Price
License     : BSD-3-Clause
Maintainer  : fprice@pricemail.ca
Stability   : experimental
Portability : POSIX

Command line utility and library to convert Google Takeout Location data to KML format 
-}
module Data.Location.GoogleLocationSpec (spec) where

import qualified Data.Location.GoogleLocation as GL
import qualified Data.Location.Model as M
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Data.Time.Clock
import NeatInterpolation (text)
import Test.Hspec

spec :: Spec
spec = do
    describe "Data.Location.GoogleLocation.getLocationFromByteString" $ do
        it "Converts a JSON ByteString to a LocationRecord list" $ do
            (GL.getLocationRecordsFromByteString (cs googleLocationJSON)) `shouldBe` locationList

locationList :: [M.LocationRecord]
locationList = [typicalLocationAllFields, typicalLocationNoAltitude, typicalLocationNoAccuracy, typicalLocationNoOptionalFields]

typicalDate :: UTCTime
typicalDate = read "2023-12-25 18:28:52.607875 UTC" :: UTCTime

typicalPositiveLongitudeNumber :: Int
typicalPositiveLongitudeNumber = 447405071

typicalNegativeLatitudeNumber :: Int
typicalNegativeLatitudeNumber = -798735599

typicalLocationAllFields :: M.LocationRecord
typicalLocationAllFields = M.LocationRecord typicalDate typicalPositiveLongitudeNumber typicalNegativeLatitudeNumber (Just 183) (Just 13)

typicalLocationNoAltitude :: M.LocationRecord
typicalLocationNoAltitude = M.LocationRecord (GL.addDaysUTCTime 1 typicalDate) typicalPositiveLongitudeNumber typicalNegativeLatitudeNumber Nothing (Just 13)

typicalLocationNoAccuracy :: M.LocationRecord
typicalLocationNoAccuracy = M.LocationRecord (GL.addDaysUTCTime 2 typicalDate) typicalPositiveLongitudeNumber typicalNegativeLatitudeNumber (Just 183) Nothing

typicalLocationNoOptionalFields :: M.LocationRecord
typicalLocationNoOptionalFields = M.LocationRecord (GL.addDaysUTCTime 3 typicalDate) typicalPositiveLongitudeNumber typicalNegativeLatitudeNumber Nothing Nothing

googleLocationJSON :: T.Text
googleLocationJSON =
    [text|
    {
    "locations": [
        {
        "latitudeE7": 447405071,
        "longitudeE7": -798735599,
        "accuracy": 13,
        "altitude": 183,
        "verticalAccuracy": 1,
        "activity": [
            {
            "activity": [
                {
                "type": "UNKNOWN",
                "confidence": 41
                },
                {
                "type": "IN_VEHICLE",
                "confidence": 10
                },
                {
                "type": "ON_BICYCLE",
                "confidence": 10
                },
                {
                "type": "ON_FOOT",
                "confidence": 10
                },
                {
                "type": "STILL",
                "confidence": 10
                },
                {
                "type": "WALKING",
                "confidence": 10
                },
                {
                "type": "RUNNING",
                "confidence": 10
                },
                {
                "type": "IN_ROAD_VEHICLE",
                "confidence": 10
                },
                {
                "type": "IN_RAIL_VEHICLE",
                "confidence": 10
                }
            ],
            "timestamp": "2023-11-13T02:04:08.588Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 96
                },
                {
                "type": "ON_FOOT",
                "confidence": 2
                },
                {
                "type": "WALKING",
                "confidence": 2
                },
                {
                "type": "IN_VEHICLE",
                "confidence": 1
                },
                {
                "type": "UNKNOWN",
                "confidence": 1
                },
                {
                "type": "IN_ROAD_VEHICLE",
                "confidence": 1
                },
                {
                "type": "IN_RAIL_VEHICLE",
                "confidence": 1
                }
            ],
            "timestamp": "2023-11-13T02:05:09.509Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:06:09.664Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:07:10.477Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:08:10.571Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:09:10.730Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:10:10.850Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:11:11.439Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:12:11.531Z"
            }
        ],
        "source": "WIFI",
        "deviceTag": 623593388,
        "platformType": "ANDROID",
        "activeWifiScan": {
            "accessPoints": [
            {
                "mac": "82179071244990",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "82179071244985",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "82179071244987",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "82179071244991",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "185122854537800",
                "strength": -64,
                "frequencyMhz": 2457
            },
            {
                "mac": "64561708141601",
                "strength": -66,
                "frequencyMhz": 2412
            },
            {
                "mac": "99746080230433",
                "strength": -66,
                "frequencyMhz": 2412
            },
            {
                "mac": "185122854537804",
                "strength": -72,
                "isConnected": true,
                "frequencyMhz": 5745
            },
            {
                "mac": "253712216230437",
                "strength": -76,
                "frequencyMhz": 2412
            },
            {
                "mac": "268886088754700",
                "strength": -76,
                "frequencyMhz": 2437
            },
            {
                "mac": "101694050488993",
                "strength": -77,
                "frequencyMhz": 2437
            },
            {
                "mac": "64561708141602",
                "strength": -78,
                "frequencyMhz": 5785
            },
            {
                "mac": "101694050488987",
                "strength": -78,
                "frequencyMhz": 2437
            },
            {
                "mac": "101694050488988",
                "strength": -78,
                "frequencyMhz": 2437
            },
            {
                "mac": "101694050488992",
                "strength": -78,
                "frequencyMhz": 2437
            },
            {
                "mac": "203019708105986",
                "strength": -81,
                "frequencyMhz": 2412
            },
            {
                "mac": "239309056430",
                "strength": -82,
                "frequencyMhz": 2437
            },
            {
                "mac": "11715607655038",
                "strength": -82,
                "frequencyMhz": 2462
            },
            {
                "mac": "11715607655035",
                "strength": -83,
                "frequencyMhz": 2462
            },
            {
                "mac": "11715607655036",
                "strength": -83,
                "frequencyMhz": 2462
            },
            {
                "mac": "165589415991794",
                "strength": -84,
                "frequencyMhz": 2452
            },
            {
                "mac": "158992346225138",
                "strength": -85,
                "frequencyMhz": 2452
            },
            {
                "mac": "275483158521357",
                "strength": -88,
                "frequencyMhz": 5745
            },
            {
                "mac": "233569152951437",
                "strength": -92,
                "frequencyMhz": 2437
            },
            {
                "mac": "158992346225214",
                "strength": -92,
                "frequencyMhz": 2452
            },
            {
                "mac": "235761187381727",
                "strength": -92,
                "frequencyMhz": 2462
            },
            {
                "mac": "64561710254137",
                "strength": -92,
                "frequencyMhz": 2462
            },
            {
                "mac": "165589415991870",
                "strength": -93,
                "frequencyMhz": 2452
            },
            {
                "mac": "218169001337311",
                "strength": -93,
                "frequencyMhz": 2462
            },
            {
                "mac": "233562156108681",
                "strength": -95,
                "frequencyMhz": 2462
            },
            {
                "mac": "226965086342025",
                "strength": -95,
                "frequencyMhz": 2462
            }
            ]
        },
        "osLevel": 34,
        "serverTimestamp": "2023-12-25T02:12:22.763Z",
        "deviceTimestamp": "2023-12-25T02:12:21.707Z",
        "batteryCharging": false,
        "formFactor": "PHONE",
        "timestamp": "2023-12-25T18:28:52.607875Z"
        },
        {
        "latitudeE7": 447405071,
        "longitudeE7": -798735599,
        "accuracy": 13,
        "verticalAccuracy": 1,
        "activity": [
            {
            "activity": [
                {
                "type": "UNKNOWN",
                "confidence": 41
                },
                {
                "type": "IN_VEHICLE",
                "confidence": 10
                },
                {
                "type": "ON_BICYCLE",
                "confidence": 10
                },
                {
                "type": "ON_FOOT",
                "confidence": 10
                },
                {
                "type": "STILL",
                "confidence": 10
                },
                {
                "type": "WALKING",
                "confidence": 10
                },
                {
                "type": "RUNNING",
                "confidence": 10
                },
                {
                "type": "IN_ROAD_VEHICLE",
                "confidence": 10
                },
                {
                "type": "IN_RAIL_VEHICLE",
                "confidence": 10
                }
            ],
            "timestamp": "2023-11-13T02:04:08.588Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 96
                },
                {
                "type": "ON_FOOT",
                "confidence": 2
                },
                {
                "type": "WALKING",
                "confidence": 2
                },
                {
                "type": "IN_VEHICLE",
                "confidence": 1
                },
                {
                "type": "UNKNOWN",
                "confidence": 1
                },
                {
                "type": "IN_ROAD_VEHICLE",
                "confidence": 1
                },
                {
                "type": "IN_RAIL_VEHICLE",
                "confidence": 1
                }
            ],
            "timestamp": "2023-11-13T02:05:09.509Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:06:09.664Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:07:10.477Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:08:10.571Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:09:10.730Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:10:10.850Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:11:11.439Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:12:11.531Z"
            }
        ],
        "source": "WIFI",
        "deviceTag": 623593388,
        "platformType": "ANDROID",
        "activeWifiScan": {
            "accessPoints": [
            {
                "mac": "82179071244990",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "82179071244985",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "82179071244987",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "82179071244991",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "185122854537800",
                "strength": -64,
                "frequencyMhz": 2457
            },
            {
                "mac": "64561708141601",
                "strength": -66,
                "frequencyMhz": 2412
            },
            {
                "mac": "99746080230433",
                "strength": -66,
                "frequencyMhz": 2412
            },
            {
                "mac": "185122854537804",
                "strength": -72,
                "isConnected": true,
                "frequencyMhz": 5745
            },
            {
                "mac": "253712216230437",
                "strength": -76,
                "frequencyMhz": 2412
            },
            {
                "mac": "268886088754700",
                "strength": -76,
                "frequencyMhz": 2437
            },
            {
                "mac": "101694050488993",
                "strength": -77,
                "frequencyMhz": 2437
            },
            {
                "mac": "64561708141602",
                "strength": -78,
                "frequencyMhz": 5785
            },
            {
                "mac": "101694050488987",
                "strength": -78,
                "frequencyMhz": 2437
            },
            {
                "mac": "101694050488988",
                "strength": -78,
                "frequencyMhz": 2437
            },
            {
                "mac": "101694050488992",
                "strength": -78,
                "frequencyMhz": 2437
            },
            {
                "mac": "203019708105986",
                "strength": -81,
                "frequencyMhz": 2412
            },
            {
                "mac": "239309056430",
                "strength": -82,
                "frequencyMhz": 2437
            },
            {
                "mac": "11715607655038",
                "strength": -82,
                "frequencyMhz": 2462
            },
            {
                "mac": "11715607655035",
                "strength": -83,
                "frequencyMhz": 2462
            },
            {
                "mac": "11715607655036",
                "strength": -83,
                "frequencyMhz": 2462
            },
            {
                "mac": "165589415991794",
                "strength": -84,
                "frequencyMhz": 2452
            },
            {
                "mac": "158992346225138",
                "strength": -85,
                "frequencyMhz": 2452
            },
            {
                "mac": "275483158521357",
                "strength": -88,
                "frequencyMhz": 5745
            },
            {
                "mac": "233569152951437",
                "strength": -92,
                "frequencyMhz": 2437
            },
            {
                "mac": "158992346225214",
                "strength": -92,
                "frequencyMhz": 2452
            },
            {
                "mac": "235761187381727",
                "strength": -92,
                "frequencyMhz": 2462
            },
            {
                "mac": "64561710254137",
                "strength": -92,
                "frequencyMhz": 2462
            },
            {
                "mac": "165589415991870",
                "strength": -93,
                "frequencyMhz": 2452
            },
            {
                "mac": "218169001337311",
                "strength": -93,
                "frequencyMhz": 2462
            },
            {
                "mac": "233562156108681",
                "strength": -95,
                "frequencyMhz": 2462
            },
            {
                "mac": "226965086342025",
                "strength": -95,
                "frequencyMhz": 2462
            }
            ]
        },
        "osLevel": 34,
        "serverTimestamp": "2023-12-26T02:12:22.763Z",
        "deviceTimestamp": "2023-12-26T02:12:21.707Z",
        "batteryCharging": false,
        "formFactor": "PHONE",
        "timestamp": "2023-12-26T18:28:52.607875Z"
        },
        {
        "latitudeE7": 447405071,
        "longitudeE7": -798735599,
        "altitude": 183,
        "activity": [
            {
            "activity": [
                {
                "type": "UNKNOWN",
                "confidence": 41
                },
                {
                "type": "IN_VEHICLE",
                "confidence": 10
                },
                {
                "type": "ON_BICYCLE",
                "confidence": 10
                },
                {
                "type": "ON_FOOT",
                "confidence": 10
                },
                {
                "type": "STILL",
                "confidence": 10
                },
                {
                "type": "WALKING",
                "confidence": 10
                },
                {
                "type": "RUNNING",
                "confidence": 10
                },
                {
                "type": "IN_ROAD_VEHICLE",
                "confidence": 10
                },
                {
                "type": "IN_RAIL_VEHICLE",
                "confidence": 10
                }
            ],
            "timestamp": "2023-11-13T02:04:08.588Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 96
                },
                {
                "type": "ON_FOOT",
                "confidence": 2
                },
                {
                "type": "WALKING",
                "confidence": 2
                },
                {
                "type": "IN_VEHICLE",
                "confidence": 1
                },
                {
                "type": "UNKNOWN",
                "confidence": 1
                },
                {
                "type": "IN_ROAD_VEHICLE",
                "confidence": 1
                },
                {
                "type": "IN_RAIL_VEHICLE",
                "confidence": 1
                }
            ],
            "timestamp": "2023-11-13T02:05:09.509Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:06:09.664Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:07:10.477Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:08:10.571Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:09:10.730Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:10:10.850Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:11:11.439Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:12:11.531Z"
            }
        ],
        "source": "WIFI",
        "deviceTag": 623593388,
        "platformType": "ANDROID",
        "activeWifiScan": {
            "accessPoints": [
            {
                "mac": "82179071244990",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "82179071244985",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "82179071244987",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "82179071244991",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "185122854537800",
                "strength": -64,
                "frequencyMhz": 2457
            },
            {
                "mac": "64561708141601",
                "strength": -66,
                "frequencyMhz": 2412
            },
            {
                "mac": "99746080230433",
                "strength": -66,
                "frequencyMhz": 2412
            },
            {
                "mac": "185122854537804",
                "strength": -72,
                "isConnected": true,
                "frequencyMhz": 5745
            },
            {
                "mac": "253712216230437",
                "strength": -76,
                "frequencyMhz": 2412
            },
            {
                "mac": "268886088754700",
                "strength": -76,
                "frequencyMhz": 2437
            },
            {
                "mac": "101694050488993",
                "strength": -77,
                "frequencyMhz": 2437
            },
            {
                "mac": "64561708141602",
                "strength": -78,
                "frequencyMhz": 5785
            },
            {
                "mac": "101694050488987",
                "strength": -78,
                "frequencyMhz": 2437
            },
            {
                "mac": "101694050488988",
                "strength": -78,
                "frequencyMhz": 2437
            },
            {
                "mac": "101694050488992",
                "strength": -78,
                "frequencyMhz": 2437
            },
            {
                "mac": "203019708105986",
                "strength": -81,
                "frequencyMhz": 2412
            },
            {
                "mac": "239309056430",
                "strength": -82,
                "frequencyMhz": 2437
            },
            {
                "mac": "11715607655038",
                "strength": -82,
                "frequencyMhz": 2462
            },
            {
                "mac": "11715607655035",
                "strength": -83,
                "frequencyMhz": 2462
            },
            {
                "mac": "11715607655036",
                "strength": -83,
                "frequencyMhz": 2462
            },
            {
                "mac": "165589415991794",
                "strength": -84,
                "frequencyMhz": 2452
            },
            {
                "mac": "158992346225138",
                "strength": -85,
                "frequencyMhz": 2452
            },
            {
                "mac": "275483158521357",
                "strength": -88,
                "frequencyMhz": 5745
            },
            {
                "mac": "233569152951437",
                "strength": -92,
                "frequencyMhz": 2437
            },
            {
                "mac": "158992346225214",
                "strength": -92,
                "frequencyMhz": 2452
            },
            {
                "mac": "235761187381727",
                "strength": -92,
                "frequencyMhz": 2462
            },
            {
                "mac": "64561710254137",
                "strength": -92,
                "frequencyMhz": 2462
            },
            {
                "mac": "165589415991870",
                "strength": -93,
                "frequencyMhz": 2452
            },
            {
                "mac": "218169001337311",
                "strength": -93,
                "frequencyMhz": 2462
            },
            {
                "mac": "233562156108681",
                "strength": -95,
                "frequencyMhz": 2462
            },
            {
                "mac": "226965086342025",
                "strength": -95,
                "frequencyMhz": 2462
            }
            ]
        },
        "osLevel": 34,
        "serverTimestamp": "2023-12-27T02:12:22.763Z",
        "deviceTimestamp": "2023-12-27T02:12:21.707Z",
        "batteryCharging": false,
        "formFactor": "PHONE",
        "timestamp": "2023-12-27T18:28:52.607875Z"
        },
        {
        "latitudeE7": 447405071,
        "longitudeE7": -798735599,
        "activity": [
            {
            "activity": [
                {
                "type": "UNKNOWN",
                "confidence": 41
                },
                {
                "type": "IN_VEHICLE",
                "confidence": 10
                },
                {
                "type": "ON_BICYCLE",
                "confidence": 10
                },
                {
                "type": "ON_FOOT",
                "confidence": 10
                },
                {
                "type": "STILL",
                "confidence": 10
                },
                {
                "type": "WALKING",
                "confidence": 10
                },
                {
                "type": "RUNNING",
                "confidence": 10
                },
                {
                "type": "IN_ROAD_VEHICLE",
                "confidence": 10
                },
                {
                "type": "IN_RAIL_VEHICLE",
                "confidence": 10
                }
            ],
            "timestamp": "2023-11-13T02:04:08.588Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 96
                },
                {
                "type": "ON_FOOT",
                "confidence": 2
                },
                {
                "type": "WALKING",
                "confidence": 2
                },
                {
                "type": "IN_VEHICLE",
                "confidence": 1
                },
                {
                "type": "UNKNOWN",
                "confidence": 1
                },
                {
                "type": "IN_ROAD_VEHICLE",
                "confidence": 1
                },
                {
                "type": "IN_RAIL_VEHICLE",
                "confidence": 1
                }
            ],
            "timestamp": "2023-11-13T02:05:09.509Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:06:09.664Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:07:10.477Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:08:10.571Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:09:10.730Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:10:10.850Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:11:11.439Z"
            },
            {
            "activity": [
                {
                "type": "STILL",
                "confidence": 100
                }
            ],
            "timestamp": "2023-11-13T02:12:11.531Z"
            }
        ],
        "source": "WIFI",
        "deviceTag": 623593388,
        "platformType": "ANDROID",
        "activeWifiScan": {
            "accessPoints": [
            {
                "mac": "82179071244990",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "82179071244985",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "82179071244987",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "82179071244991",
                "strength": -57,
                "frequencyMhz": 2412
            },
            {
                "mac": "185122854537800",
                "strength": -64,
                "frequencyMhz": 2457
            },
            {
                "mac": "64561708141601",
                "strength": -66,
                "frequencyMhz": 2412
            },
            {
                "mac": "99746080230433",
                "strength": -66,
                "frequencyMhz": 2412
            },
            {
                "mac": "185122854537804",
                "strength": -72,
                "isConnected": true,
                "frequencyMhz": 5745
            },
            {
                "mac": "253712216230437",
                "strength": -76,
                "frequencyMhz": 2412
            },
            {
                "mac": "268886088754700",
                "strength": -76,
                "frequencyMhz": 2437
            },
            {
                "mac": "101694050488993",
                "strength": -77,
                "frequencyMhz": 2437
            },
            {
                "mac": "64561708141602",
                "strength": -78,
                "frequencyMhz": 5785
            },
            {
                "mac": "101694050488987",
                "strength": -78,
                "frequencyMhz": 2437
            },
            {
                "mac": "101694050488988",
                "strength": -78,
                "frequencyMhz": 2437
            },
            {
                "mac": "101694050488992",
                "strength": -78,
                "frequencyMhz": 2437
            },
            {
                "mac": "203019708105986",
                "strength": -81,
                "frequencyMhz": 2412
            },
            {
                "mac": "239309056430",
                "strength": -82,
                "frequencyMhz": 2437
            },
            {
                "mac": "11715607655038",
                "strength": -82,
                "frequencyMhz": 2462
            },
            {
                "mac": "11715607655035",
                "strength": -83,
                "frequencyMhz": 2462
            },
            {
                "mac": "11715607655036",
                "strength": -83,
                "frequencyMhz": 2462
            },
            {
                "mac": "165589415991794",
                "strength": -84,
                "frequencyMhz": 2452
            },
            {
                "mac": "158992346225138",
                "strength": -85,
                "frequencyMhz": 2452
            },
            {
                "mac": "275483158521357",
                "strength": -88,
                "frequencyMhz": 5745
            },
            {
                "mac": "233569152951437",
                "strength": -92,
                "frequencyMhz": 2437
            },
            {
                "mac": "158992346225214",
                "strength": -92,
                "frequencyMhz": 2452
            },
            {
                "mac": "235761187381727",
                "strength": -92,
                "frequencyMhz": 2462
            },
            {
                "mac": "64561710254137",
                "strength": -92,
                "frequencyMhz": 2462
            },
            {
                "mac": "165589415991870",
                "strength": -93,
                "frequencyMhz": 2452
            },
            {
                "mac": "218169001337311",
                "strength": -93,
                "frequencyMhz": 2462
            },
            {
                "mac": "233562156108681",
                "strength": -95,
                "frequencyMhz": 2462
            },
            {
                "mac": "226965086342025",
                "strength": -95,
                "frequencyMhz": 2462
            }
            ]
        },
        "osLevel": 34,
        "serverTimestamp": "2023-12-28T02:12:22.763Z",
        "deviceTimestamp": "2023-12-28T02:12:21.707Z",
        "batteryCharging": false,
        "formFactor": "PHONE",
        "timestamp": "2023-12-28T18:28:52.607875Z"
        }
    ]
    }
    |]
