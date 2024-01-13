{-# LANGUAGE Unsafe #-}

module Main (main) where

import Data.Location.GoogleLocation as GL
import Data.Location.KML as KML

import Data.Time

import CmdOptions

import System.IO

import Data.ByteString.Builder as B

main :: IO ()
main = do
    -- Get the configuration data from command line parameters
    configuration <- getConfiguration

    -- Get the location records from the Google Takout file
    locationList <- GL.getLocationRecordsFromFilePath (inputFilename configuration)

    -- Filter records by date if required
    now <- getCurrentTime
    let listToOutput = case filterOlderThanDays configuration of
            Nothing -> locationList
            Just x -> GL.filterOlderThan filterDate locationList
              where
                filterDate = GL.addDaysUTCTime ((-1) * x) now

    -- Output as KML
    case outputFilename configuration of
        Nothing -> B.hPutBuilder stdout (KML.renderKML listToOutput)
        Just x -> B.writeFile x (KML.renderKML listToOutput)
