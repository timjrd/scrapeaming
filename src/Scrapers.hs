module Scrapers (sources, video) where

import System.FilePath

import Environment
import Scraper

type Url = String

video :: Scraper
video = Scraper
  { name       = "video"
  , timeout    = 120
  , maxOutputs = 5
  , runAt      = DocumentStart
  , allFrames  = True
  , args       = [] }

sources :: String -> IO [(Url,Scraper)]
sources query = do
  root <- getRoot
  file <- readFile $ root </> "scrapers" </> "sources.txt"
  return $ map (fmap toScraper) $ parse file
  where
    parse        = concatMap (toPair . words) . lines
    toPair [a,b] = [(b,a)]
    toPair _     = []
    toScraper x  = Scraper
      { name       = x
      , timeout    = 120
      , maxOutputs = 100
      , runAt      = DocumentIdle
      , allFrames  = False
      , args       = [("QUERY", query)] }
