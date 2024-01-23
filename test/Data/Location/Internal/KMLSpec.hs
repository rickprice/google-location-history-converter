{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
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

import Relude

import Data.Location.Internal.KML
import Data.Location.Model
import Data.Time.Clock
import Test.Hspec
import Text.Read

spec :: Spec
spec = do
    describe "Data.Location.Internal.KML.xmlGISHeader" $
        it "Returns a KML Header string" $
            xmlKMLHeader `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>\n"

    describe "Data.Location.Internal.KML.xmlGISFooter" $
        it "Returns a KML Footer string" $
            xmlKMLFooter `shouldBe` "</Document></kml>"

    describe "Data.Location.Internal.KML.wrapMaybeTag" $ do
        it "Handles an Integer value by returning it wrapped with a <MyData> tag" $
            wrapMaybeTag "MyData" (Just "value") `shouldBe` Just "<MyData>value</MyData>"

        it "Handles a Nothing value by returning Nothing" $
            wrapMaybeTag "MyData" Nothing `shouldBe` Nothing

    describe "Data.Location.Internal.KML.wrapMaybeDataTag" $ do
        it "Handles an Integer value by returning it wrapped with an appropriate tag set" $
            wrapMaybeDataTag "MyData" (Just 777) `shouldBe` Just "<Data name=\"MyData\"><value>777</value></Data>"

        it "Handles a Nothing value by returning Nothing" $
            wrapMaybeDataTag "MyData" Nothing `shouldBe` Nothing

    describe "Data.Location.Internal.KML.wrapExtendedValues" $ do
        it "Handles a Location with all values by converting it to an XML Snippet" $
            wrapExtendedValues typicalLocationAllFields `shouldBe` Just "<ExtendedData><Data name=\"accuracy\"><value>10</value></Data><Data name=\"altitude\"><value>126</value></Data></ExtendedData>"
        --
        it "Handles a Location with all values but Altitude by converting it to an XML Snippet" $
            wrapExtendedValues typicalLocationNoAltitude `shouldBe` Just "<ExtendedData><Data name=\"accuracy\"><value>10</value></Data></ExtendedData>"

        it "Handles a Location with all values but Accuracy by converting it to an XML Snippet" $
            wrapExtendedValues typicalLocationNoAccuracy `shouldBe` Just "<ExtendedData><Data name=\"altitude\"><value>126</value></Data></ExtendedData>"

        it "Handles a Location without optional values by converting it to an XML Snippet" $
            wrapExtendedValues typicalLocationNoOptionalFields `shouldBe` Nothing

    describe "Data.Location.Internal.KML.toPlacemarkDataTag" $
        it "Returns a correctly formatted Placemark snippet from a Location" $
            toPlacemarkDataTag typicalLocationAllFields `shouldBe` "<Placemark><TimeStamp><when>2023-12-25T18:28:52.607875Z</when></TimeStamp><ExtendedData><Data name=\"accuracy\"><value>10</value></Data><Data name=\"altitude\"><value>126</value></Data></ExtendedData><Point><coordinates>-79.8735599,44.7405071</coordinates></Point></Placemark>\n"

    describe "Data.Location.Internal.KML.convertLongitudeToBuilder" $ do
        it "Returns a correct string for a positive regular Longitude Number" $
            convertLongitudeToBuilder typicalNegativeLongitudeNumber `shouldBe` "-79.8735599"
        it "Returns a correct string for small positive Longitude Number" $
            convertLongitudeToBuilder smallPositiveLongitudeNumber `shouldBe` "0.0005071"
        it "Returns a correct string for small positive Longitude Number" $
            convertLongitudeToBuilder smallNegativeLongitudeNumber `shouldBe` "-0.0005071"

    describe "Data.Location.Internal.KML.convertLatitudeToBuilder" $
        it "Returns a correct string for a negative regular Latitude Number" $
            convertLatitudeToBuilder typicalPositiveLatitudeNumber `shouldBe` "44.7405071"

typicalDate :: UTCTime
typicalDate = read "2023-12-25 18:28:52.607875 UTC" :: UTCTime

typicalPositiveLatitudeNumber :: Latitude
typicalPositiveLatitudeNumber = fromE6IntegerLatitude 447405071

typicalNegativeLongitudeNumber :: Longitude
typicalNegativeLongitudeNumber = fromE6IntegerLongitude (-798735599)

smallPositiveLongitudeNumber :: Longitude
smallPositiveLongitudeNumber = fromE6IntegerLongitude 5071

smallNegativeLongitudeNumber :: Longitude
smallNegativeLongitudeNumber = fromE6IntegerLongitude (-5071)

typicalLocationAllFields :: LocationRecord
typicalLocationAllFields = LocationRecord typicalDate typicalPositiveLatitudeNumber typicalNegativeLongitudeNumber (Just 126) (Just 10)

typicalLocationNoAltitude :: LocationRecord
typicalLocationNoAltitude = LocationRecord typicalDate typicalPositiveLatitudeNumber typicalNegativeLongitudeNumber Nothing (Just 10)

typicalLocationNoAccuracy :: LocationRecord
typicalLocationNoAccuracy = LocationRecord typicalDate typicalPositiveLatitudeNumber typicalNegativeLongitudeNumber (Just 126) Nothing

typicalLocationNoOptionalFields :: LocationRecord
typicalLocationNoOptionalFields = LocationRecord typicalDate typicalPositiveLatitudeNumber typicalNegativeLongitudeNumber Nothing Nothing
