{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      : Data.Location.GoogleLocation
Description : Google Takeout Location to KML Converter
Copyright   : (c) 2024 Frederick Price
License     : BSD-3-Clause
Maintainer  : fprice@pricemail.ca
Stability   : experimental
Portability : POSIX

Module to parse Google Takeout Location records as JSON and convert them to Location records
-}
module Data.Location.GoogleLocation (
    -- * Overview
    -- $overview

    -- * Converters
    getLocationRecordsFromFilePath,
    getLocationRecordsFromByteString,

    -- * Utility functions
    filterOlderThan,
    addDaysUTCTime,
) where

import Relude

import Data.Location.Model

import qualified Codec.Archive.Tar as Tar

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS

import qualified Data.JsonStream.Parser as J

import Data.Time.Clock

{- | This is like the standard 'foldr' function on lists, but for 'Entries'.
 Compared to 'foldEntries' it skips failures.
-}
foldEntriesIgnoreFailure :: (Tar.Entry -> a -> a) -> a -> Tar.Entries e -> a
foldEntriesIgnoreFailure next done = fold'
  where
    fold' (Tar.Next e es) = next e (fold' es)
    fold' Tar.Done = done
    fold' (Tar.Fail _) = done

-- Convert an entry to its ByteString
entryToByteString :: Tar.Entry -> BS.ByteString
entryToByteString entry = extractFileData (Tar.entryContent entry)
  where
    -- Extract the file data here case
    extractFileData (Tar.NormalFile d _) = d
    extractFileData _ = BS.empty

-- Is this Entry the location data we are looking to export
entryIsLocationData :: Tar.Entry -> Bool
entryIsLocationData e = case Tar.entryContent e of
    Tar.NormalFile _ _ -> doesPathMatch (Tar.entryPath e)
    _AnyFailure -> False
  where
    doesPathMatch :: String -> Bool
    doesPathMatch p = "Takeout/Location History (Timeline)/Records.json" == p

-- | Return a list of LocationRecords parsed from a Google Takeout Location file in .tgz format
getLocationRecordsFromFilePath :: FilePath -> IO [LocationRecord]
getLocationRecordsFromFilePath filePath = do
    fileContent <- GZip.decompress <$> readFileLBS filePath
    let entries = Tar.read fileContent
    let entryList = foldEntriesIgnoreFailure (:) [] entries
    let entryListFiltered = nonEmpty (Relude.filter entryIsLocationData entryList)
    case entryListFiltered of
        Nothing -> return []
        Just entryListNonEmpty -> return locationRecords
          where
            locationRecordFile filteredEntries = entryToByteString (Relude.head filteredEntries)
            locationRecords = getLocationRecordsFromByteString (locationRecordFile entryListNonEmpty)

-- | Return a list of LocationRecords parsed from a ByteString
getLocationRecordsFromByteString :: BS.ByteString -> [LocationRecord]
getLocationRecordsFromByteString = J.parseLazyByteString locationRecordsParser

-- | Move a UTCTime forward or backward by the given number of days
addDaysUTCTime :: Integer -> UTCTime -> UTCTime
addDaysUTCTime x = addUTCTime (nominalDay * fromIntegral x)

-- | Return a list of LocationRecords records newer than the given UTCTime
filterOlderThan :: UTCTime -> [LocationRecord] -> [LocationRecord]
filterOlderThan filterDate = Relude.filter (\x -> timestamp x > filterDate)

-- The Parser setup
locationRecordParser :: J.Parser LocationRecord
locationRecordParser =
    LocationRecord
        <$> "timestamp" J..: J.value
            <*> "latitudeE7" J..: J.value
            <*> "longitudeE7" J..: J.value
            <*> "altitude" J..:? J.integer
            <*> "accuracy" J..:? J.integer

locationRecordsParser :: J.Parser LocationRecord
locationRecordsParser =
    J.objectWithKey "locations" $ J.arrayOf locationRecordParser

{- $overview
 This module lets you convert Google Takeout Location records into Location values.
-}
