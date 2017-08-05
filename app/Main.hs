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
import           System.IO               (Handle, IOMode(AppendMode), hPutStrLn, stderr, openFile)

import           Control.Concurrent      (threadDelay)

import           Control.Monad           (forever, unless)

import           WebWatch.GetLinks


data Config = Config
    { cPatterns        :: [T.Text]
    , cUrl             :: T.Text
    , cInterval        :: Int
    , cSlackWebhookUrl :: T.Text
    } deriving (Show)



parseConfig :: C.Config -> IO Config
parseConfig conf = do
    cPatterns        <- C.require conf "patterns"
    cUrl             <- C.require conf "url"
    cInterval        <- C.require conf "interval"
    cSlackWebhookUrl <- C.require conf "slack.webhook_url"
    return Config {..}
-- parseConfig :: C.Config -> IO Config
-- parseConfig conf = C.require conf "patterns" >>= (\cPatterns -> 
--     C.require conf "url" >>= (\cUrl ->
--     C.require conf "interval" >>= (\cInterval ->
--     C.require conf "slack.webhook_url" >>= (\cSlackWebhookUrl ->
--     return Config {cPatterns=cPatterns, cUrl=cUrl, cInterval=cInterval, cSlackWebhookUrl=cSlackWebhookUrl}))))



main :: IO ()
main = getArgs >>= (\args ->
    case args of
        [confPath] -> do
          cConfig <- C.load [C.Required confPath]
          config  <- parseConfig cConfig
          webWatch config
          putStrLn "Got a config file:"
          putStrLn (show config)
        _ -> putStrLn "Usage: webwatch CONFIG" >>
            exitFailure)



type LinkSet = HS.HashSet T.Text

type WebWatchM = ReaderT Config (StateT LinkSet IO)

slog :: String -> WebWatchM ()
slog msg = liftIO $ hPutStrLn stderr msg

sFile :: Handle -> String -> WebWatchM ()
sFile handle msg = liftIO $ hPutStrLn handle msg


addLinks :: [Link] -> LinkSet -> ([Link], LinkSet)
addLinks links set =
    (new, HS.union set (HS.fromList $ map lHref new))
  where
    new = filter (\l -> not $ lHref l `HS.member` set) links

-- (_, set) = addLinks ["https://zurihac.info"] Data.HashSet.empty
-- (_, set1) = addLinks ["https://zurihac.info"] set


    
watchOnce :: WebWatchM ()
watchOnce = do
    Config {..} <- ask
    slog $ "Getting links from " ++ T.unpack cUrl

    -- TODO: Fetch the links
   
    links <- liftIO $ getMatchingLinks cPatterns cUrl
    slog $ "All links: " ++ show links

    newLinks <- state (addLinks links)
    slog $ "New links: " ++ show newLinks

    
    -- TODO: Send new links to file
    unless (null newLinks) $ do
        slog $ "Sending new links to file..."
        handle <- liftIO $ openFile "LinksFromHN.txt" AppendMode
        sFile handle (show newLinks)
        

    slog $ "Sleeping " ++ show cInterval ++ " minute(s)"
    liftIO $ threadDelay (cInterval * 60 * 1000 * 1000)



webWatch :: Config -> IO ()
webWatch config =
    evalStateT (runReaderT (forever watchOnce) config) HS.empty
