{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Unsafe #-}

module Data.Location.KML (toXMLString) where

import Data.Location.Internal.KML
import Data.Location.Model

import Prelude

toXMLString :: [LocationRecord] -> String
toXMLString x =
    xmlGISHeader
        ++ concatMap toPlacemarkDataTag x
        ++ xmlGISFooter
