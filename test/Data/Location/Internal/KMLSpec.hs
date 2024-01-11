module Data.Location.Internal.KMLSpec (spec) where

import Data.Location.Internal.KML
import Test.Hspec

spec :: Spec
spec = do
    describe "xmlGISHeader" $ do
        it "Returns a KML Header string" $ do
            xmlGISHeader `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>"

    describe "xmlGISFooter" $ do
        it "Returns a KML Footer string" $ do
            xmlGISFooter `shouldBe` "</Document></kml>"
