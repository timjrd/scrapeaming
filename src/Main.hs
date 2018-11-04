module Main where

import System.Timeout

import Process
import Task

main :: IO ()
main = withProcess "/usr/bin/env" ["bash", "-c", "echo It Works!"]
       Stderr (-1) () $ \_ line -> do
  putStr line
  return $ Just ()
