{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module CmdOptions (getConfiguration, Config(..)) where

import Options.Applicative

data Config = Config
  { inputFile      :: !String
  , outputFile      :: !String
  , filterOlderThanDays :: !Int }

config :: Parser Config
config = Config
      <$> strOption
          ( long "inputFile"
         <> metavar "FILENAME"
         <> help "Input filename - something like takeout.tgz" )
      <*> strOption
          ( long "outputFile"
         <> metavar "FILENAME"
         <> help "Output file name - something like takeout.kml" )
      <*> option auto
          ( long "filterMoreThanDays"
         <> help "Filter lecation records so they are no older than n days"
         <> showDefault
         <> value 14
         <> metavar "INT" )

runMain :: IO ()
runMain = do
    configuration <- getConfiguration
    runProgram configuration

-- runMain :: IO ()
-- runMain = runProgram =<< execParser opts
--   where
--     opts = info (config <**> helper)
--       ( fullDesc
--      <> progDesc "Convert Google Location data in .tgz format to KML"
--      <> header "Convert Google Location Data" )

runProgram :: Config -> IO ()
runProgram (Config inputFile outputFile n) = putStrLn $ "Hello, " ++ inputFile ++ replicate n '!'
runProgram _ = return ()


getConfiguration :: IO Config
getConfiguration = execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Convert Google Location data in .tgz format to KML"
     <> header "Convert Google Location Data" )

