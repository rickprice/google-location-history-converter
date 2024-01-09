{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module Main (main) where

import Data.Location.GoogleLocation as GL
import Data.Location.Model as M

import Data.Time.Clock

import CmdOptions

main :: IO ()
main = do
    configuration <- getConfiguration

    -- Get the location records from the Google Takout file
    locationList <- GL.getLocationRecords (inputFile configuration)

    -- print locationList

    -- Filter records older than two weeks
    now <- getCurrentTime
    let filterDate = addUTCTime (-nominalDay * 2 * 7) now
    let locationListFilteredByDate = GL.filterOlderThan filterDate locationList

    -- let lengthOriginal = Prelude.length locationList
    -- let lengthFilteredByDate = Prelude.length locationListFilteredByDate
    -- print locationListFilteredByDate

    -- Output as KML
    putStrLn $ toXMLString locationListFilteredByDate

-- print lengthOriginal
-- print lengthFilteredByDate

-- print "finished"
