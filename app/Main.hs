{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Unsafe #-}

module Main (main) where

import Codec.Archive.Tar as Tar

import Codec.Compression.GZip as GZip
-- import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Time.Clock
import Model
import System.Exit
import Prelude
import qualified Data.Text as T
import qualified Data.JsonStream.Parser as J
import Data.JsonStream.Parser ((.:), (.:?), (.|))

-- import Data.Time
import Control.Monad (mfilter)
import Data.ByteString qualified as BS2

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
    doesPathMatch p = "Takeout/Location History/Records.json" == p

-- location = Locations 1 (Text.pack "test") 2 3
-- model = Model [location]

jsonFile :: FilePath
jsonFile = "Takeout/Location History/Records.json"


jsonErrorsFile :: FilePath
jsonErrorsFile = "json-stream-errors-example.json"

locationRecordFile :: IO BS2.ByteString
locationRecordFile = BS2.readFile jsonFile 

-- -- Testing
-- jsonErrorsRecordFile :: IO BS2.ByteString
-- jsonErrorsRecordFile = BS2.readFile jsonErrorsFile
--
-- -- | Result of bulk operation
-- resultParser :: J.Parser [(T.Text, T.Text)]
-- resultParser =    const [] <$> filterI not ("errors" .: bool)
--               <|> many ("items" .: arrayOf bulkItemError)
--
-- bulkItemError :: J.Parser (T.Text, T.Text)
-- bulkItemError = objectWithKey "index" $
--     (,) <$> "_id"   .: string
--         <*> "error" .: string
--         <*  filterI statusError ("status" .: J.integer)
--   where
--     statusError s = s < 200 || s > (299 :: Int)
-- -- Testing



main :: IO ()
main = do
    -- d <- (eitherDecodeStrict <$> jsonErrorsRecordFile) :: IO (Either String Model)
    -- -- If d is Left, the JSON was malformed.
    -- -- In that case, we report the error.
    -- -- Otherwise, we perform the operation of
    -- -- our choice. In this case, just print it.
    -- case d of
    --     Left err -> die err
    --     Right ps -> do
    --         putStrLn $ toXMLString locationListFilteredDate

    -- -- fileContent <- GZip.decompress <$> BS.readFile "takeout.tgz"
    -- -- fileContent <- fmap GZip.decompress (BS.readFile "takeout.tgz")
    -- -- let entries = Tar.read fileContent
    -- -- let entryList = foldEntriesIgnoreFailure (:) [] entries
    -- -- -- let locationRecordFiles = map entryToByteString (filter entryIsLocationData entryList)
    -- -- -- let locationRecordFile = pure (head locationRecordFiles)
    -- -- let locationRecordFile = pure(entryToByteString (head (filter entryIsLocationData entryList)))
    -- -- -- let locationRecordFile = pure (head locationRecordFiles)
    -- -- print "starting"
    -- --
    -- -- line <- BS.hGetLine locationRecordFile
    -- --
    -- -- print line
    --
    -- -- Get JSON data and decode it
    -- -- d <- (eitherDecode <$> locationRecordFile) :: IO (Either String Model)
    -- d <- (eitherDecodeStrict <$> locationRecordFile) :: IO (Either String Model)
    -- -- If d is Left, the JSON was malformed.
    -- -- In that case, we report the error.
    -- -- Otherwise, we perform the operation of
    -- -- our choice. In this case, just print it.
    -- case d of
    --     Left err -> die err
    --     Right ps -> do
    --         now <- getCurrentTime
    --         let twoWeekAgo = addUTCTime (-nominalDay * 7 * 2) now
    --         -- let locationList = locations ps
    --         -- let locationListFilteredDropJunk = filter isComplete locationList
    --         let locationListFilteredDate = mfilter (\x -> (timestamp x > Just twoWeekAgo) && isComplete x) $ locations ps
    --         -- let lengthOriginal = length locationList
    --         -- let lengthFiltered = length locationListFiltered
    --         -- let lengthFilteredDate = length locationListFilteredDate
    --         -- print locationListFiltered
    --         -- print locationListFilteredDate
    --         -- print lengthOriginal
    --         -- print lengthFiltered
    --         -- print lengthFilteredDate
    --         putStrLn $ toXMLString locationListFilteredDate

    print "finished"
