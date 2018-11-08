module Util where

import System.Environment

getRoot :: IO String
getRoot = getEnv "SCRAPEAMING_ROOT"
