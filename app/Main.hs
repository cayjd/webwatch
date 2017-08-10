{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import           System.Environment      (getArgs)
import           System.Exit             (exitFailure)

import Data.Set (Set)
import qualified Data.Set as Set

import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Control.Monad.State     (StateT, evalStateT, state)
import           Control.Monad.Trans     (liftIO)
import           System.IO               (Handle, IOMode(AppendMode), IOMode(ReadWriteMode),
                                          hPutStrLn, hGetContents, stderr, openFile)

import           Control.Concurrent      (threadDelay)
import           Control.Monad           (forever, unless)

import           WebWatch.GetLinks



data Config = Config
    { cPatterns        :: [T.Text]
    , cUrl             :: T.Text
    , cInterval        :: Int
    , cFile            :: T.Text
    } deriving (Show)

parseConfig :: C.Config -> IO Config
parseConfig conf = do
    cPatterns        <- C.require conf "patterns"
    cUrl             <- C.require conf "url"
    cInterval        <- C.require conf "interval"
    cFile            <- C.require conf "file"
    return Config {..}


type LinkHrefSet = Set T.Text

addLinks :: [Link] -> LinkHrefSet -> ([Link], LinkHrefSet)
addLinks links set =
    (new, set `Set.union` (Set.fromList $ map lHref new))
  where
    new = filter (\l -> not $ lHref l `Set.member` set) links



type WebWatchM = ReaderT Config (StateT LinkHrefSet IO)

slog :: String -> WebWatchM ()
slog msg = liftIO $ hPutStrLn stderr msg

appendLinkFile :: Handle -> String -> WebWatchM ()
appendLinkFile handle msg = liftIO $ hPutStrLn handle msg
                            
watchOnce :: WebWatchM ()
watchOnce = do
    -- Get configuration parameters
    Config {..} <- ask
    slog $ "Getting links from: " ++ T.unpack cUrl
    slog $ "with patterns: " ++ show cPatterns
    slog $ "with IO file: " ++ T.unpack cFile
    slog $ "with interval (in min): " ++ show cInterval    

    -- TODO give more info

    -- Fetch the links
    links <- liftIO $ getMatchingLinks cPatterns cUrl
    slog $ "All links: " ++ show links

    -- Get new links
    newLinks <- state (addLinks links)
    slog $ "New links: " ++ show newLinks
    
    -- Send new links to file
    unless (null newLinks) $ do
        slog $ "Sending new links to file..."
        handle <- liftIO $ openFile (T.unpack cFile) AppendMode
        mapM_ (\l -> appendLinkFile handle (show l)) newLinks
        
    -- Sleep
    slog $ "Sleeping " ++ show cInterval ++ " minute(s)"
    liftIO $ threadDelay (cInterval * 60 * 1000 * 1000)



readLinkFile :: Handle -> IO [Link]
readLinkFile handle = do
    fileString <- hGetContents handle      
    return $ map parseLink (T.lines (T.pack fileString))

webWatch :: Config -> IO ()
webWatch config = do
    handle  <- openFile (T.unpack (cFile config)) ReadWriteMode
    initList <- readLinkFile handle
    initSet <- return $ Set.fromList $ map lHref initList
    evalStateT (runReaderT (forever watchOnce) config) initSet



main :: IO ()
main = do
    args <- getArgs
    case args of
        [confPath] -> do
            config <- parseConfig =<< C.load [C.Required confPath]
            webWatch config
        _ -> do
            putStrLn "Usage: webwatch CONFIG"
            exitFailure
