{-# OPTIONS_HADDOCK show-extensions #-}
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

Module to take Location records and output them as KML
-}
module Data.Location.KML (
-- * Overview
-- $overview

-- * Converters
toKML
) where

import Data.Location.Internal.KML
import Data.Location.Model

import Data.Monoid
import Data.Text.Lazy.Builder

import Prelude

{- | Convert a list of LocationRecords into a String Builder, in the KML format-}
toKML :: [LocationRecord] -> Builder
toKML x = xmlKMLHeader <> foldMap toPlacemarkDataTag x <> xmlKMLFooter

{- $overview
 This module lets you convert Location records into KML suitable for use with Exiftool
-}
