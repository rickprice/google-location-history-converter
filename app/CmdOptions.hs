{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      : CmdOptions
Description : Google Takeout Location to KML Converter
Copyright   : (c) 2024 Frederick Price
License     : BSD-3-Clause
Maintainer  : fprice@pricemail.ca
Stability   : experimental
Portability : POSIX

Command line utility and library to convert Google Takeout Location data to KML format
-}
module CmdOptions (getConfiguration, Config (..)) where

import Relude

import Options.Applicative

data Config = Config
    { inputFilename :: !String
    , outputFilename :: !(Maybe String)
    , filterOlderThanDays :: !(Maybe Integer)
    }

config :: Parser Config
config =
    Config
        <$> strOption
            ( long "inputFile"
                <> metavar "FILENAME"
                <> help "Input filename - something like takeout.tgz"
            )
        <*> optional
            ( strOption
                ( long "outputFile"
                    <> metavar "FILENAME"
                    <> help "Output file name - something like takeout.kml"
                )
            )
        <*> optional
            ( option
                auto
                ( long "filterMoreThanDays"
                    <> help "Filter lecation records so they are no older than n days"
                    -- <> showDefault
                    -- <> value 14
                    <> metavar "INT"
                )
            )

getConfiguration :: IO Config
getConfiguration = execParser opts
  where
    opts =
        info
            (config <**> helper)
            ( fullDesc
                <> progDesc "Convert Google Location data in .tgz format to KML"
                <> header "Convert Google Location Data"
            )
