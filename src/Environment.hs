module Environment where

import System.Environment

import GHC.Conc

envPrefix = "SCRAPEAMING_"

getMyEnv    = getEnv    . (envPrefix++)
lookupMyEnv = lookupEnv . (envPrefix++)

getRoot     = getMyEnv "ROOT"
getChromium = getMyEnv "CHROMIUM"
getXvfb     = getMyEnv "XVFB"
getFFprobe  = getMyEnv "FFPROBE"

getJobs :: IO Int
getJobs = do
  mjobs <- lookupMyEnv "JOBS"
  case mjobs of
    Just jobs -> return $ read jobs
    Nothing -> do
      cores <- getNumProcessors
      return $ cores * 2
