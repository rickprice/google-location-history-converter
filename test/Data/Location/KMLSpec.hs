{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      : Data.Location.KMLSpec
Description : Google Takeout Location to KML Converter
Copyright   : (c) 2024 Frederick Price
License     : BSD-3-Clause
Maintainer  : fprice@pricemail.ca
Stability   : experimental
Portability : POSIX

Command line utility and library to convert Google Takeout Location data to KML format
-}
module Data.Location.KMLSpec (spec) where

import Relude

import qualified Data.Text.Lazy.Builder as B

import Data.Location.KML
import Data.Location.Model
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Data.Time.Clock
import NeatInterpolation (text)
import Test.Hspec
import Text.Read

spec :: Spec
spec = do
    describe "Data.Location.KML.xmlGISHeader" $ do
        it "Converts a list of Locations to XML" $ do
            -- (cs (toLazyByteString (toKML locationList))) `shouldBe` locationListKMLAsString
            B.toLazyText (toKML locationList) `shouldBe` cs locationListKMLAsString

locationList :: [LocationRecord]
locationList = [typicalLocationAllFields, typicalLocationNoAccuracy, typicalLocationNoAltitude, typicalLocationNoOptionalFields]

typicalDate :: UTCTime
typicalDate = read "2023-12-25 18:28:52.607875 UTC" :: UTCTime

typicalPositiveLongitudeNumber :: Int
typicalPositiveLongitudeNumber = 447405071

typicalNegativeLatitudeNumber :: Int
typicalNegativeLatitudeNumber = -798735599

typicalLocationAllFields :: LocationRecord
typicalLocationAllFields = LocationRecord typicalDate typicalPositiveLongitudeNumber typicalNegativeLatitudeNumber (Just 126) (Just 10)

typicalLocationNoAltitude :: LocationRecord
typicalLocationNoAltitude = LocationRecord typicalDate typicalPositiveLongitudeNumber typicalNegativeLatitudeNumber Nothing (Just 10)

typicalLocationNoAccuracy :: LocationRecord
typicalLocationNoAccuracy = LocationRecord typicalDate typicalPositiveLongitudeNumber typicalNegativeLatitudeNumber (Just 126) Nothing

typicalLocationNoOptionalFields :: LocationRecord
typicalLocationNoOptionalFields = LocationRecord typicalDate typicalPositiveLongitudeNumber typicalNegativeLatitudeNumber Nothing Nothing

-- Original example from json-stream
locationListKMLAsString :: T.Text
locationListKMLAsString =
    [text|
    <?xml version="1.0" encoding="UTF-8"?><kml xmlns="http://www.opengis.net/kml/2.2"><Document><name>Location History</name>
    <Placemark><TimeStamp><when>2023-12-25T18:28:52.607875Z</when></TimeStamp><ExtendedData><Data name="accuracy"><value>10</value></Data><Data name="altitude"><value>126</value></Data></ExtendedData><Point><coordinates>-79.8735599,44.7405071</coordinates></Point></Placemark>
    <Placemark><TimeStamp><when>2023-12-25T18:28:52.607875Z</when></TimeStamp><ExtendedData><Data name="altitude"><value>126</value></Data></ExtendedData><Point><coordinates>-79.8735599,44.7405071</coordinates></Point></Placemark>
    <Placemark><TimeStamp><when>2023-12-25T18:28:52.607875Z</when></TimeStamp><ExtendedData><Data name="accuracy"><value>10</value></Data></ExtendedData><Point><coordinates>-79.8735599,44.7405071</coordinates></Point></Placemark>
    <Placemark><TimeStamp><when>2023-12-25T18:28:52.607875Z</when></TimeStamp><Point><coordinates>-79.8735599,44.7405071</coordinates></Point></Placemark>
    </Document></kml>
    |]
