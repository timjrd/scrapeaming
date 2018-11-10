module Logger
  ( Logger
  , Log(..)
  , LogTag(..)
  , newLogger ) where

import Control.Monad.STM
import Control.Concurrent.STM.TQueue

import Control.Concurrent.Async

type Url = String

data LogTag = Important | Info | Warning | Error

data Log = Log LogTag String [Url]

type Logger = Log -> IO ()

newLogger :: IO (Logger, IO Log)
newLogger = do
  logs <- atomically $ newTQueue
  let logger  = atomically . writeTQueue logs
  let nextLog = atomically $ readTQueue  logs
  return (logger,nextLog)
