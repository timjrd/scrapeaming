{-# LANGUAGE QuasiQuotes #-}
module Scraper
  ( Scraper(Scraper)
  , RunAt(DocumentStart, DocumentEnd, DocumentIdle)
  , withScraper ) where

import System.FilePath
import System.IO
import System.IO.Temp

import Util
import Task

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
  , allFrames  :: Bool }

withScraper :: Scraper -> (Task String String -> IO a) -> IO a
withScraper scraper f = do
  dir  <- fmap (</> "scrapers") getRoot
  head <- readFile $ dir </> "head.js"
  src  <- readFile $ dir </> name scraper <.> "js"
  withSystemTempDirectory extName $ \ext -> do
    writeFile (ext </> "manifest.json") manifest
    writeFile (ext </> "script.js") $ unlines [head,src]
    f undefined -- TODO
  where
    extName = "scrapeaming-scraper-" ++ name scraper
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
      

  
