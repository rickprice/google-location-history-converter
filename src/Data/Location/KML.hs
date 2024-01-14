{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Unsafe #-}

module Data.Location.KML (renderKML) where

import Data.Location.Internal.KML
import Data.Location.Model

-- import Data.ByteString.Builder

import Data.Monoid
import Data.Text.Lazy.Builder

import Prelude

renderKML :: [LocationRecord] -> Builder
renderKML x = xmlKMLHeader <> foldMap toPlacemarkDataTag x <> xmlKMLFooter
