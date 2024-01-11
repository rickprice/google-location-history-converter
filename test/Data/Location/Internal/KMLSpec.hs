module Data.Location.Internal.KMLSpec (spec) where

import Data.Location.Internal.KML
import Data.Location.Model
import Data.Time.Clock
import Test.Hspec


spec :: Spec
spec = do
    describe "Data.Location.Internal.KML.xmlGISHeader" $ do
        it "Returns a KML Header string" $ do
            xmlGISHeader `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>"

    describe "Data.Location.Internal.KML.xmlGISFooter" $ do
        it "Returns a KML Footer string" $ do
            xmlGISFooter `shouldBe` "</Document></kml>"

    describe "Data.Location.Internal.KML.wrapWithDataTag" $ do
        it "Handles a Nothing value by returning an empty string" $ do
            wrapWithDataTag "MyData" Nothing `shouldBe` ""

        it "Handles an Integer value by returning it wrapped with a <Data> tag" $ do
            wrapWithDataTag "MyData" (Just 777) `shouldBe` "<Data name=\"MyData\"><value>777</value></Data>"

    describe "Data.Location.Internal.KML.toExtendedDataTag" $ do
        it "Handles a Location with all values by converting it to an XML Snippet" $ do
            toExtendedDataTag typicalLocationAllFields `shouldBe` "<ExtendedData><Data name=\"accuracy\"><value>10</value></Data><Data name=\"altitude\"><value>126</value></Data></ExtendedData>"

        it "Handles a Location with all values but Altitude by converting it to an XML Snippet" $ do
            toExtendedDataTag typicalLocationNoAltitude `shouldBe` "<ExtendedData><Data name=\"accuracy\"><value>10</value></Data></ExtendedData>"

        it "Handles a Location with all values but Accuracy by converting it to an XML Snippet" $ do
            toExtendedDataTag typicalLocationNoAccuracy `shouldBe` "<ExtendedData><Data name=\"altitude\"><value>126</value></Data></ExtendedData>"

        it "Handles a Location without optional values by converting it to an XML Snippet" $ do
            toExtendedDataTag typicalLocationNoOptionalFields `shouldBe` ""

typicalDate::UTCTime
typicalDate=read "2023-12-25 18:28:52.607875 UTC" :: UTCTime

typicalLocationAllFields::LocationRecord
typicalLocationAllFields = LocationRecord typicalDate 447405071 (-798735599) (Just 126) (Just 10)

typicalLocationNoAltitude::LocationRecord
typicalLocationNoAltitude= LocationRecord typicalDate 447405071 (-798735599) Nothing (Just 10)

typicalLocationNoAccuracy::LocationRecord
typicalLocationNoAccuracy = LocationRecord typicalDate 447405071 (-798735599) (Just 126) Nothing

typicalLocationNoOptionalFields::LocationRecord
typicalLocationNoOptionalFields = LocationRecord typicalDate  447405071 (-798735599) Nothing Nothing

