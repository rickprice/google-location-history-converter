module Data.Location.GoogleLocation (
    getLocationRecords,
    filterOlderThan,
) where

import Data.Location.Model as M

import Codec.Archive.Tar as Tar

import Codec.Compression.GZip as GZip
import Data.ByteString.Lazy as BS

import qualified Data.JsonStream.Parser as J

import Data.Time.Clock

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

getLocationRecords :: FilePath -> IO [LocationRecord]
getLocationRecords filePath = do
    fileContent <- GZip.decompress <$> BS.readFile filePath
    let entries = Tar.read fileContent
    let entryList = foldEntriesIgnoreFailure (:) [] entries
    let locationRecordFile = entryToByteString (Prelude.head (Prelude.filter entryIsLocationData entryList))

    return (J.parseLazyByteString M.locationRecordsParser locationRecordFile)

filterOlderThan :: UTCTime -> [LocationRecord] -> [LocationRecord]
filterOlderThan filterDate = Prelude.filter (\x -> timestamp x > filterDate)
