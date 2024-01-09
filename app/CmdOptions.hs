{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module CmdOptions (runMain) where

import Options.Applicative

data Config = Config
  { inputFile      :: String
  , outputFile      :: String
  , noOlderThanDays :: Int }

config :: Parser Config
config = Config
      <$> strOption
          ( long "inputFile"
         <> metavar "TARGET"
         <> help "Inputfilename" )
      <*> strOption
          ( long "outputFile"
         <> metavar "TARGET"
         <> help "Output file name" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

runMain :: IO ()
runMain = greet =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Config -> IO ()
greet (Config inputFile outputFile n) = putStrLn $ "Hello, " ++ inputFile ++ replicate n '!'
greet _ = return ()
