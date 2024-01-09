-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module Main (main) where

import Data.Location.GoogleLocation as GL
import Data.Location.Model as M

import Data.Time
import Data.Time.Clock

import CmdOptions

addDaysUTCTime :: UTCTime -> Integer -> UTCTime
addDaysUTCTime t x = addUTCTime (nominalDay * (fromIntegral x)) t

main :: IO ()
main = do
    configuration <- getConfiguration

    -- Get the location records from the Google Takout file
    locationList <- GL.getLocationRecords (inputFile configuration)

    -- print locationList

    -- Filter records older than two weeks
    now <- getCurrentTime
    let filterDate = addDaysUTCTime now ((-1) * (filterOlderThanDays configuration))
    let locationListFilteredByDate = GL.filterOlderThan filterDate locationList

    -- let lengthOriginal = Prelude.length locationList
    -- let lengthFilteredByDate = Prelude.length locationListFilteredByDate
    -- print locationListFilteredByDate

    -- Output as KML
    putStrLn $ toXMLString locationListFilteredByDate

-- print lengthOriginal
-- print lengthFilteredByDate

-- print "finished"
