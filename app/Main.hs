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
import           Control.Concurrent      (threadDelay)
import           Control.Monad           (forever, unless, when)
import           System.IO               (hPutStrLn, stderr)

import WebWatch.GetLinks

data Config = Config
    { cPatterns        :: [T.Text]
    , cUrl             :: T.Text
    , cInterval        :: Int
    , cSlackWebhookUrl :: T.Text
    , cFilename        :: T.Text
    } deriving (Show)

parseConfig :: C.Config -> IO Config
parseConfig conf = C.require conf "patterns" >>= (\cPatterns -> 
    C.require conf "url" >>= (\cUrl ->
    C.require conf "interval" >>= (\cInterval ->
    C.require conf "slack.webhook_url" >>= (\cSlackWebhookUrl ->
    C.require conf "filename" >>= (\cFilename ->
    return Config {cPatterns=cPatterns, cUrl=cUrl, cInterval=cInterval, cSlackWebhookUrl=cSlackWebhookUrl, cFilename=cFilename})))))

type LinkSet = HS.HashSet T.Text
type WebWatchM = ReaderT Config (StateT LinkSet IO)

slog :: String -> WebWatchM ()
slog msg = liftIO $ hPutStrLn stderr msg

addLinks :: [Link] -> LinkSet -> ([Link], LinkSet)
addLinks links set =
    (new, HS.union set (HS.fromList $ map lHref new))
  where
    new = filter (\l -> not $ (lHref l) `HS.member` set) links

watchOnce :: WebWatchM ()
watchOnce = ask >>= (\config ->
    (slog $ "Getting links from " ++ T.unpack (cUrl config)) >>
    (liftIO $ getMatchingLinks (cPatterns config) (cUrl config)) >>= (\links ->
    (slog $ "All matching links: " ++ show links) >>
    ((state $ addLinks links) >>= (\newLinks -> slog $ "New Links: " ++ (show newLinks))) >>
    -- TODO: Send message
    (slog $ "Sleeping " ++ show (cInterval config) ++ " microsecond(s)") >>
    (liftIO $ writeLinks config links) >>

    (liftIO $ threadDelay (cInterval config))))

main :: IO ()
main = getArgs >>= (\args ->
    case args of
        [confPath] -> C.load [C.Required confPath] >>= 
            parseConfig >>= 
            (\c -> (showConfig c >> webWatch c))

        _ -> putStrLn "Usage: webwatch CONFIG" >>
            exitFailure)

showConfig :: Config -> IO ()
showConfig config = 
    putStrLn "Got a config file:" >>
    putStrLn (show config)

webWatch :: Config -> IO ()
webWatch config =
    evalStateT (runReaderT (forever watchOnce) config) HS.empty

writeLinks :: Config -> [Link] -> IO ()
writeLinks config links = writeFile (T.unpack $ cFilename config) (show links)
