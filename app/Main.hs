{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module Main (main) where

import Data.Location.GoogleLocation as GL
import Data.Location.Model as M

import Data.Time.Clock

import CmdOptions

main :: IO ()
main = do
    runMain

    -- Get the location records from the Google Takout file
    locationList <- GL.getLocationRecords "takeout.tgz"

    -- print locationList

    -- Filter records older than two weeks
    now <- getCurrentTime
    let twoWeeksAgo = addUTCTime (-nominalDay * 7 * 2) now
    let locationListFilteredByDate = GL.filterOlderThan twoWeeksAgo locationList

    -- let lengthOriginal = Prelude.length locationList
    -- let lengthFilteredByDate = Prelude.length locationListFilteredByDate
    -- print locationListFilteredByDate

    -- Output as KML
    putStrLn $ toXMLString locationListFilteredByDate

-- print lengthOriginal
-- print lengthFilteredByDate

-- print "finished"
