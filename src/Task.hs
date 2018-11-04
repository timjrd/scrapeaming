module Task where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TSem
import Control.Concurrent.STM.TQueue

type Task a b = a -> (b -> IO ()) -> IO ()

bind :: Task a b -> Task b c -> Task a c
bind t1 t2 input output = t1 input $ \x ->
  t2 x output

fork :: Task a b -> Int -> Task b c -> Task a c
fork t1 jobs t2 input output = do
  sem <- atomically $ newTSem jobs
  t1 input $ \x -> do
    atomically $ waitTSem sem
    void $ forkIO $ do
      t2 x output
      atomically $ signalTSem sem

merge :: Task a b -> c -> Task (b,c) c -> Task a c
merge t1 init t2 input output = do
  queue <- atomically newTQueue
  let f acc = do
        x <- atomically $ readTQueue queue
        t2 (x,acc) f
  forkIO $ f init
  t1 input $ atomically . writeTQueue queue
