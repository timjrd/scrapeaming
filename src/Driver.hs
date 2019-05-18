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
import qualified Scrapers
import Probe

type Driver a = String -> (IO (Maybe (Either Log Result)) -> IO a) -> IO a

withDriver :: (Driver a -> IO b) -> IO b
withDriver f = withDisplay $ f . driver

driver :: Int -> Driver a
driver display query f = do
  jobs       <- getJobs
  (log,logs) <- newLogger
  sources    <- Scrapers.sources query
  withScrapers display log (map snd sources)
    $ \srcs  -> withScraper display log Scrapers.video
    $ \video ->
        roundRobin (zipWith (\a b -> feed [[a]] b) (map fst sources) srcs)
        `bind`     dedupOn head
        `bind`     spread jobs video
        `bind`     dedupOn head
        `bind`     spread jobs (probe log)
        `withTask` \out -> withInputs logs out f

addResult :: Result -> [Result] -> [Result]
addResult x xs | x `elem` xs = xs
               | otherwise   = sortBy (flip compare) $ x : xs
