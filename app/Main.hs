{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import           System.Environment      (getArgs)
import           System.Exit             (exitFailure)

import qualified Data.HashSet as HS
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Control.Monad.State     (StateT, evalStateT, state)
import           Control.Monad.Trans     (liftIO)
import           System.IO               (hPutStrLn, stderr)

data Config = Config
    { cPatterns        :: [T.Text]
    , cUrl             :: T.Text
    , cInterval        :: Int
    , cSlackWebhookUrl :: T.Text
    } deriving (Show)

parseConfig :: C.Config -> IO Config
parseConfig conf = C.require conf "patterns" >>= (\cPatterns -> 
    C.require conf "url" >>= (\cUrl ->
    C.require conf "interval" >>= (\cInterval ->
    C.require conf "slack.webhook_url" >>= (\cSlackWebhookUrl ->
    return Config {cPatterns=cPatterns, cUrl=cUrl, cInterval=cInterval, cSlackWebhookUrl=cSlackWebhookUrl}))))

main :: IO ()
main = getArgs >>= (\args ->
    case args of
        [confPath] -> showConfigFromPath confPath
        _ -> putStrLn "Usage: webwatch CONFIG" >>
            exitFailure)

showConfigFromPath :: String -> IO ()
showConfigFromPath confPath = C.load [C.Required confPath] >>= (\cConfig ->
    parseConfig cConfig >>= (\config ->
    putStrLn "Got a config file:" >>
    putStrLn (show config)))

type LinkSet = HS.HashSet T.Text
type WebWatchM = ReaderT Config (StateT LinkSet IO)

slog :: String -> WebWatchM ()
slog msg = liftIO $ hPutStrLn stderr msg
