module Driver
  ( Driver
  , Result
  , Log(..)
  , LogTag(..)
  , withDriver
  , forInput
  , foldInput
  , addResult
  , sources
  , quality
  , duration
  , width
  , height
  , title ) where

import Data.List

import Environment
import Task
import Display
import Logger
import Scraper
import Probe

type Url = String

type Driver a = String -> (IO (Maybe (Either Log Result)) -> IO a) -> IO a

withDriver :: (Driver a -> IO b) -> IO b
withDriver f = withDisplay $ f . driver

sourceScrapers =
  [ ("google"    , "https://www.google.com/")
  , ("duckduckgo", "https://duckduckgo.com/") ]

sourceScrapers' :: String -> [Scraper]
sourceScrapers' query = map f sourceScrapers
  where
    f (x,_) = Scraper
      { name       = x
      , timeout    = 60
      , maxOutputs = 30
      , runAt      = DocumentIdle
      , allFrames  = False
      , args       = [("QUERY", query)] }

videoScraper = Scraper
  { name       = "video"
  , timeout    = 60
  , maxOutputs = 5
  , runAt      = DocumentStart
  , allFrames  = True
  , args       = [] }

driver :: Int -> Driver a
driver display query f = do
  jobs       <- getJobs
  (log,logs) <- newLogger
  let spread' = spread jobs
  withScrapers display log (sourceScrapers' query)
    $ \srcs  -> withScraper display log videoScraper
    $ \video ->
        roundRobin (zipWith feed' srcs sourceScrapers)
        `spread'`  video
        `spread'`  (probe log)
        `withTask` \out -> withInputs logs out f
  where
    feed' src (_,init) = feed [[init]] src

addResult :: Result -> [Result] -> [Result]
addResult x xs | x `elem` xs = xs
               | otherwise   = sortBy (flip compare) $ x : xs
