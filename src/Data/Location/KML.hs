{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Unsafe #-}

{-|
Module      : Data.Location.KML
Description : Google Takeout Location to KML Converter
Copyright   : (c) 2024 Frederick Price
License     : BSD-3-Clause
Maintainer  : fprice@pricemail.ca
Stability   : experimental
Portability : POSIX

Command line utility and library to convert Google Takeout Location data to KML format 
-}
module Data.Location.KML (renderKML) where

import Data.Location.Internal.KML
import Data.Location.Model

-- import Data.ByteString.Builder

import Data.Monoid
import Data.Text.Lazy.Builder

import Prelude

renderKML :: [LocationRecord] -> Builder
renderKML x = xmlKMLHeader <> foldMap toPlacemarkDataTag x <> xmlKMLFooter
