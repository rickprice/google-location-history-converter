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
module Data.Location.KML (toKML) where

import Data.Location.Internal.KML
import Data.Location.Model

import Data.Monoid
import Data.Text.Lazy.Builder

import Prelude

toKML :: [LocationRecord] -> Builder
toKML x = xmlKMLHeader <> foldMap toPlacemarkDataTag x <> xmlKMLFooter
