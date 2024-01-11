module Data.Location.Internal.KMLSpec (spec) where

import Test.Hspec
import Data.Location.Internal.KML

spec :: Spec
spec = do
  describe "xmlGISFooter" $ do
    it "Returns a KML Footer string" $ do
      xmlGISFooter `shouldBe` "</Document></kml>"
