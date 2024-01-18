{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

import Relude

import Data.Location.Internal.KML
import Data.Location.Model

import Data.Text.Lazy.Builder

{- | Convert a list of LocationRecords into a String Builder, in the KML format-}
toKML :: [LocationRecord] -> Builder
toKML x = xmlKMLHeader <> foldMap toPlacemarkDataTag x <> xmlKMLFooter

{- $overview
 This module lets you convert Location records into KML suitable for use with Exiftool
-}
