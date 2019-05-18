module Scraper
  ( Scraper(..)
  , RunAt(..)
  , withScrapers
  , withScraper ) where

import Control.Monad
import Control.Exception

import Data.Function ((&))
import Data.List
import Data.IORef
import qualified Data.Set as S

import System.FilePath
import qualified System.Timeout as TO

import Environment
import Task
import Temp
import Process
import Token
import Logger

type Url = String

data RunAt = DocumentStart | DocumentEnd | DocumentIdle

instance Show RunAt where
  show DocumentStart = "document_start"
  show DocumentEnd   = "document_end"
  show DocumentIdle  = "document_idle"

data Scraper = Scraper
  { name       :: String
  , timeout    :: Int
  , maxOutputs :: Int
  , runAt      :: RunAt
  , allFrames  :: Bool
  , args       :: [(String,String)] }

withScrapers :: Int -> Logger -> [Scraper] -> ([Task [Url] [Url]] -> IO a) -> IO a
withScrapers display log scrapers f = g scrapers []
  where
    g    []  ys = f $ reverse ys
    g (x:xs) ys = withScraper display log x $ \y -> g xs (y:ys)

withScraper :: Int -> Logger -> Scraper -> (Task [Url] [Url] -> IO a) -> IO a
withScraper display log scraper f = do
  token <- getToken
  let args' = foldMap arg $ ("TOKEN",token) : args scraper
  dir  <- fmap (</> "scrapers") getRoot
  head <- readFile $ dir </> "head.js"
  src  <- readFile $ dir </> name scraper <.> "js"
  withSystemTempDirectory' extName $ \ext -> do
    writeFile (ext </> "manifest.json") manifest
    writeFile (ext </> "script.js") $ unlines [args', head, src]
    f $ task display log scraper ext token
  where
    extName = "scrapeaming-scraper-" ++ name scraper
    arg (k,v) = "const " ++ k ++ " = " ++ show v ++ ";\n"
    runAt' = show $ runAt scraper
    allFrames' = if allFrames scraper then "true" else "false"
    tr a b = map $ \x -> if x == a then b else x
    manifest = tr '\'' '"' $ unlines
      [ "{ 'manifest_version': 2,"
      , "  'name': '" ++ extName ++ "',"
      , "  'version': '1.0',"
      , "  'content_scripts': [{"
      , "    'matches': ['<all_urls>'],"
      , "    'run_at': '" ++ runAt' ++ "',"
      , "    'all_frames': " ++ allFrames' ++ ","
      , "    'js': ['script.js']"
      , "  }] }" ]

task :: Int -> Logger -> Scraper -> FilePath -> String -> Task [Url] [Url]
task display log scraper ext token input output = do
  chromium <- getChromium
  withSystemTempDirectory' "scrapeaming-chromium"
    $ \tmp -> forInput input
    $ \xs  -> void $ do
    found  <- newIORef False
    result <- job chromium tmp xs found
    found' <- readIORef found
    case result of
      Nothing -> if found'
        then log' Info    "has timed out with some data" xs
        else log' Warning "has timed out with no data"   xs
      Just n | n > maxOutputs scraper -> log' Info  "has collected enough data" xs
             | otherwise              -> log' Error "was interrupted"           xs
             
  where
    log' tag msg trace = log $ Log tag (name scraper ++ " scraper " ++ msg) trace
    
    job chromium tmp xs found = withProcess chromium
      [ "--disable-gpu"
      , "--enable-logging=stderr"
      , "--display=:"       ++ show display
      , "--user-data-dir="  ++ tmp
      , "--load-extension=" ++ ext
      , head xs ]
      $ \_ _ herr -> TO.timeout (timeout scraper * 10^6)
                     $ fst <$> hFold herr (0, S.empty) (processLine found xs)
    
    processLine found xs (n,ys) line = case my of
      Just y | y `S.notMember` ys -> uninterruptibleMask_ $ do
        output (y:xs)
        writeIORef found True
        log' Info "found some url" (y:xs)
        return (n+1 <= maxOutputs scraper, (n+1, S.insert y ys))
      _ -> return (True,(n,ys))
      where
        my = words line
          & dropWhile (/=token)
          & uncons
          & fmap snd
          & fmap (takeWhile (/=token))
          & fmap unwords

