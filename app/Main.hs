{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module Main (main) where

import Data.Location.GoogleLocation as GL
import Data.Location.Model as M

import Data.Time.Clock

main :: IO ()
main = do
    locationList <- GL.getLocationRecords "takeout.tgz"

    -- print locationList

    now <- getCurrentTime
    let twoWeeksAgo = addUTCTime (-nominalDay * 7 * 2) now
    let locationListFilteredByDate = GL.filterOlderThan twoWeeksAgo locationList
    -- let lengthOriginal = Prelude.length locationList
    -- let lengthFilteredByDate = Prelude.length locationListFilteredByDate
    -- print locationListFilteredByDate
    putStrLn $ toXMLString locationListFilteredByDate

-- print lengthOriginal
-- print lengthFilteredByDate

-- print "finished"
