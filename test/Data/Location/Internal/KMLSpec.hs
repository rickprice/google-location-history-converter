{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Location.Internal.KMLSpec
Description : Google Takeout Location to KML Converter
Copyright   : (c) 2024 Frederick Price
License     : BSD-3-Clause
Maintainer  : fprice@pricemail.ca
Stability   : experimental
Portability : POSIX

Command line utility and library to convert Google Takeout Location data to KML format 
-}
module Data.Location.Internal.KMLSpec (spec) where

import Data.Location.Internal.KML
import Data.Location.Model
import Data.Time.Clock
import Test.Hspec

-- import qualified Data.Aeson.Key as xmlKMLHeader

spec :: Spec
spec = do
    describe "Data.Location.Internal.KML.xmlGISHeader" $ do
        it "Returns a KML Header string" $ do
            xmlKMLHeader `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>\n"

    describe "Data.Location.Internal.KML.xmlGISFooter" $ do
        it "Returns a KML Footer string" $ do
            xmlKMLFooter `shouldBe` "</Document></kml>"

    describe "Data.Location.Internal.KML.wrapWithDataTag" $ do
        it "Handles an Integer value by returning it wrapped with a <Data> tag" $ do
            wrapWithDataTag ("MyData", 777) `shouldBe` "<Data name=\"MyData\"><value>777</value></Data>"

    describe "Data.Location.Internal.KML.toExtendedDataTag" $ do
        it "Handles a Location with all values by converting it to an XML Snippet" $ do
            toExtendedDataTag typicalLocationAllFields `shouldBe` "<ExtendedData><Data name=\"accuracy\"><value>10</value></Data><Data name=\"altitude\"><value>126</value></Data></ExtendedData>"

    it "Handles a Location with all values but Altitude by converting it to an XML Snippet" $ do
        toExtendedDataTag typicalLocationNoAltitude `shouldBe` "<ExtendedData><Data name=\"accuracy\"><value>10</value></Data></ExtendedData>"

    it "Handles a Location with all values but Accuracy by converting it to an XML Snippet" $ do
        toExtendedDataTag typicalLocationNoAccuracy `shouldBe` "<ExtendedData><Data name=\"altitude\"><value>126</value></Data></ExtendedData>"

    it "Handles a Location without optional values by converting it to an XML Snippet" $ do
        toExtendedDataTag typicalLocationNoOptionalFields `shouldBe` ""

    describe "Data.Location.Internal.KML.toPlacemarkDataTag" $ do
        it "Returns a correctly formatted Placemark snippet from a Location" $ do
            toPlacemarkDataTag typicalLocationAllFields `shouldBe` "<Placemark><TimeStamp><when>2023-12-25T18:28:52.607875Z</when></TimeStamp><ExtendedData><Data name=\"accuracy\"><value>10</value></Data><Data name=\"altitude\"><value>126</value></Data></ExtendedData><Point><coordinates>-79.8735599,44.7405071</coordinates></Point></Placemark>\n"

    describe "Data.Location.Internal.KML.convertLocation" $ do
        it "Returns a correct string for a positive regular Longitude Number" $ do
            convertLocationToBuilder typicalPositiveLongitudeNumber `shouldBe` "44.7405071"
        it "Returns a correct string for a negative regular Latitude Number" $ do
            convertLocationToBuilder typicalNegativeLatitudeNumber `shouldBe` "-79.8735599"
        it "Returns a correct string for small positive Longitude Number" $ do
            convertLocationToBuilder smallPositiveLongitudeNumber `shouldBe` "0.0005071"
        it "Returns a correct string for small positive Longitude Number" $ do
            convertLocationToBuilder smallNegativeLongitudeNumber `shouldBe` "-0.0005071"

typicalDate :: UTCTime
typicalDate = read "2023-12-25 18:28:52.607875 UTC" :: UTCTime

typicalPositiveLongitudeNumber :: Int
typicalPositiveLongitudeNumber = 447405071

smallPositiveLongitudeNumber :: Int
smallPositiveLongitudeNumber = 5071

smallNegativeLongitudeNumber :: Int
smallNegativeLongitudeNumber = (-5071)

typicalNegativeLatitudeNumber :: Int
typicalNegativeLatitudeNumber = (-798735599)

typicalLocationAllFields :: LocationRecord
typicalLocationAllFields = LocationRecord typicalDate typicalPositiveLongitudeNumber typicalNegativeLatitudeNumber (Just 126) (Just 10)

typicalLocationNoAltitude :: LocationRecord
typicalLocationNoAltitude = LocationRecord typicalDate typicalPositiveLongitudeNumber typicalNegativeLatitudeNumber Nothing (Just 10)

typicalLocationNoAccuracy :: LocationRecord
typicalLocationNoAccuracy = LocationRecord typicalDate typicalPositiveLongitudeNumber typicalNegativeLatitudeNumber (Just 126) Nothing

typicalLocationNoOptionalFields :: LocationRecord
typicalLocationNoOptionalFields = LocationRecord typicalDate typicalPositiveLongitudeNumber typicalNegativeLatitudeNumber Nothing Nothing
