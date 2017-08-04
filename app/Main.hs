{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import           System.Environment      (getArgs)
import           System.Exit             (exitFailure)

data Config = Config
    { cPatterns        :: [T.Text]
    , cUrl             :: !T.Text
    , cInterval        :: !Int
    , cSlackWebhookUrl :: !T.Text
    } deriving (Show)

parseConfig :: C.Config -> IO Config
parseConfig conf = do
    cPatterns        <- C.require conf "patterns"
    cUrl             <- C.require conf "url"
    cInterval        <- C.require conf "interval"
    cSlackWebhookUrl <- C.require conf "slack.webhook_url"
    return Config {..}


main :: IO ()
main = do
    args <- getArgs
    case args of
        [confPath] -> do
            config <- parseConfig =<< C.load [C.Required confPath]
            putStrLn $ "Got a config file:"
            putStrLn $ show config
        _ -> do
            putStrLn "Usage: webwatch CONFIG"
            exitFailure
