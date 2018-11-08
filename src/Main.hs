module Main where

import Control.Monad
import Control.Concurrent

import System.IO.Temp
import System.Timeout

import Process
import Task
import Scraper

-- DEBUG

withChromium f = withSystemTempDirectory "chromium" $ \dir -> withProcess
  "/home/timjrd/.nix-profile/bin/chromium"
  ["--enable-logging=stderr", "--user-data-dir=" ++ dir]
  f

main :: IO ()
main = withChromium $ \_ _ herr -> do
  forkIO $ withChromium $ \_ _ herr -> hFold herr () $ \_ line -> putStrLn line >> return (True,())
  timeout (5*10^6) $ hFold herr () $ \_ line -> putStrLn line >> return (True,())
  return ()
