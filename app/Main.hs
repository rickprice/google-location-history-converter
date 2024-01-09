{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module Main (main) where

import Data.Location.Model as M

import Data.Time.Clock

import Data.Location.GoogleLocation

main :: IO ()
main = do
    locationList <- getLocationRecords "takeout.tgz"

    -- print locationList

    now <- getCurrentTime
    let twoWeeksAgo = addUTCTime (-nominalDay * 7 * 2) now
    let locationListFilteredByDate = filterOlderThan twoWeeksAgo locationList
    -- let lengthOriginal = Prelude.length locationList
    -- let lengthFilteredByDate = Prelude.length locationListFilteredByDate
    -- print locationListFilteredByDate
    putStrLn $ toXMLString locationListFilteredByDate

-- print lengthOriginal
-- print lengthFilteredByDate

-- print "finished"
