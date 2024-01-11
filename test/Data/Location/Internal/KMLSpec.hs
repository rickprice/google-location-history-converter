module Data.Location.Internal.KMLSpec (spec) where

import Data.Location.Internal.KML
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

        it "Handles a Integer value by returning it wrapped with a <Data> tag" $ do
            wrapWithDataTag "MyData" (Just 777) `shouldBe` "<Data name=\"MyData\"><value>777</value></Data>"
