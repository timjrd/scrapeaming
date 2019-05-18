module Browser (main0, main1, main2) where

import Data.List

import Task
import Scraper
import qualified Scrapers

type Url = String

main0 :: IO ()
main0 = main nop

main1 :: String -> IO ()
main1 url = main (url, stayAlive Scrapers.video)

main2 :: String -> String -> IO ()
main2 source query = do
  sources <- Scrapers.sources query
  case find ((source==) . name . snd) sources of
    Nothing -> putStrLn $ "source scraper \"" ++ source ++ "\" not found"
    Just x  -> main $ stayAlive <$> x

main :: (Url,Scraper) -> IO ()
main (url,scraper) =
  withScraper 0 (const $ return ()) scraper
  $ \task  -> feed [[url]] task (return Nothing)
  $ \(x:_) -> putStrLn x

nop :: (Url,Scraper)
nop = ( "https://github.com/timjrd/scrapeaming"
      , stayAlive Scrapers.video { name = "nop" } )

stayAlive :: Scraper -> Scraper
stayAlive x = x { timeout    = -1
                , maxOutputs = maxBound }
