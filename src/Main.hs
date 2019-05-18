module Main where

import Control.Concurrent
import Control.Exception

import System.Posix.Signals
import System.Environment

import Text.Read

import qualified Cli
import qualified Server
import qualified Browser

main = do
  tid <- myThreadId
  let handler = Catch $ throwTo tid UserInterrupt
  installHandler keyboardSignal handler Nothing
  installHandler sigTERM        handler Nothing
  
  args <- getArgs
  case args of
    ["browser"               ] -> Browser.main0
    ["browser", url          ] -> Browser.main1 url
    ["browser", source, query] -> Browser.main2 source query
    
    ["serve", port] -> case readMaybe port of
      Just p  -> Server.main p
      Nothing -> usage
      
    [query] -> Cli.main query
    
    _ -> usage

usage = do
  cmd <- getProgName
  putStr $ unlines
    [ "usage: " ++ cmd ++ " QUERY"
    , "       " ++ cmd ++ " serve PORT"
    , "       " ++ cmd ++ " browser"
    , "       " ++ cmd ++ " browser VIDEO_SCRAPER_URL"
    , "       " ++ cmd ++ " browser SOURCE_SCRAPER QUERY" ]
