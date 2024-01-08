-- {-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Unsafe #-}

module Main (main) where

-- import Model

import Control.Applicative (many, (<|>))
import qualified Data.JsonStream.Parser as J
-- import qualified Data.Text as T
-- import NeatInterpolation (text)

-- import Data.JsonStream.Parser (parseByteString)
-- import Data.JsonStream.Parser (parseByteString)

-- import Data.ByteString as BS
import Data.Time (UTCTime)

-- import Data.ByteString.Lazy as BL
-- import Data.ByteString as BS
-- import Data.Text as TS
-- import Data.Text.Lazy as TL
-- import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
-- import Data.ByteString.UTF8 as BSU      -- from utf8-string
-- import Data.Text.Encoding as TSE

import Codec.Archive.Tar as Tar

import Codec.Compression.GZip as GZip
import Data.ByteString.Lazy as BS

-- import Data.Text.Lazy.Encoding as TLE

{- | This is like the standard 'foldr' function on lists, but for 'Entries'.
 Compared to 'foldEntries' it skips failures.
-}
foldEntriesIgnoreFailure :: (Tar.Entry -> a -> a) -> a -> Tar.Entries e -> a
foldEntriesIgnoreFailure next done = fold
  where
    fold (Tar.Next e es) = next e (fold es)
    fold Tar.Done = done
    fold (Tar.Fail _) = done

-- Convert an entry to its filepath
-- entryToPath :: Tar.Entry -> String
-- entryToPath entry = show $ Tar.entryPath entry

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
    _ -> False
  where
    doesPathMatch :: String -> Bool
    doesPathMatch p = "Takeout/Location History (Timeline)/Records.json" == p
main :: IO ()
main = do
    fileContent <- GZip.decompress <$> BS.readFile "takeout.tgz"
    -- fileContent <- fmap GZip.decompress (BS.readFile "takeout.tgz")
    let entries = Tar.read fileContent
    let entryList = foldEntriesIgnoreFailure (:) [] entries
    let locationRecordFile = entryToByteString (Prelude.head (Prelude.filter entryIsLocationData entryList))

    print "starting"

    print (J.parseLazyByteString resultParserIP locationRecordFile)

    print "finished"

-- | Result of bulk operation
resultParserIP :: J.Parser [(UTCTime, Int, Int, Int, Int)]
resultParserIP = many ("locations" J..: J.arrayOf bulkItemErrorIP)

bulkItemErrorIP :: J.Parser (UTCTime, Int, Int, Int, Int)
bulkItemErrorIP =
    J.objectOf $
        (,,,,) <$> "timestamp" J..: J.value
            <*> "latitudeE7" J..: J.integer
            <*> "longitudeE7" J..: J.integer
            <*> "altitude" J..: J.integer
            <*> "accuracy" J..: J.integer
            -- <* J.filterI statusError ("status" J..: J.integer)
  -- where
    -- statusError s = s < 200 || s > (299 :: Int)

