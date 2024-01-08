{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module Main (main) where

import Model as M

import Control.Applicative (many)
import Data.JsonStream.Parser ((.:), (.:?), (.|))
import qualified Data.JsonStream.Parser as J

import Data.Time (UTCTime)

import Codec.Archive.Tar as Tar

import Codec.Compression.GZip as GZip
import Data.ByteString.Lazy as BS

{- | This is like the standard 'foldr' function on lists, but for 'Entries'.
 Compared to 'foldEntries' it skips failures.
-}
foldEntriesIgnoreFailure :: (Tar.Entry -> a -> a) -> a -> Tar.Entries e -> a
foldEntriesIgnoreFailure next done = fold
  where
    fold (Tar.Next e es) = next e (fold es)
    fold Tar.Done = done
    fold (Tar.Fail _) = done

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

locationListParserX :: J.Parser [(UTCTime, Int, Int, Int, Int)]
locationListParserX = many ("locations" J..: J.arrayOf locationParserX)

locationParserX :: J.Parser (UTCTime, Int, Int, Int, Int)
locationParserX =
    J.objectOf $
        (,,,,) <$> "timestamp" J..: J.value
            <*> "latitudeE7" J..: J.integer
            <*> "longitudeE7" J..: J.integer
            <*> "altitude" J..: J.integer
            <*> "accuracy" J..: J.integer

-- <* J.filterI statusError ("status" J..: J.integer)
-- where
-- statusError s = s < 200 || s > (299 :: Int)

locationRecordParser :: J.Parser M.LocationRecord
locationRecordParser =
    M.LocationRecord
        <$> "timestamp" J..: J.value
            <*> "latitudeE7" J..: J.integer
            <*> "longitudeE7" J..: J.integer
            <*> "altitude" J..: J.integer
            <*> "accuracy" J..: J.integer

locationRecordsParser :: J.Parser M.LocationRecord
locationRecordsParser =
    J.objectWithKey "locations" $ J.arrayOf locationRecordParser

main :: IO ()
main = do
    fileContent <- GZip.decompress <$> BS.readFile "takeout.tgz"
    -- fileContent <- fmap GZip.decompress (BS.readFile "takeout.tgz")
    let entries = Tar.read fileContent
    let entryList = foldEntriesIgnoreFailure (:) [] entries
    let locationRecordFile = entryToByteString (Prelude.head (Prelude.filter entryIsLocationData entryList))

    print "starting"

    -- print (J.parseLazyByteString locationListParserX locationRecordFile)
    print (J.parseLazyByteString locationRecordsParser locationRecordFile)

    print "finished"
