module Task
  ( Task
  , feed
  , roundRobin
  , spread
  , withTask
  , forInput
  , foldInput ) where

import Control.Monad

import Control.Monad.STM
import Control.Concurrent.STM.TQueue

import Control.Concurrent.Async

type Task a b = IO (Maybe a) -> (b -> IO ()) -> IO ()

feed :: [a] -> Task a b -> Task () b
feed xs task _ output = do
  inQ <- atomically $ newTQueue
  mapM_ (atomically . writeTQueue inQ . Just) xs
  task (atomically $ readQ inQ) output

roundRobin :: [Task () b] -> Task () b
roundRobin tasks _ output = do
  (jobs,outQs) <- fmap unzip $ forM tasks $ \task -> do
    outQ <- atomically $ newTQueue
    let job = do
          task (return Nothing) (atomically . writeTQueue outQ . Just)
          atomically $ writeTQueue outQ Nothing
    return (job,outQ)
    
  mapConcurrently_ id jobs
    `concurrently_` collect [] outQs
  where
    collect []   [] = return ()
    collect next [] = collect [] $ reverse next
    collect next (q:qs) = do
      mx <- atomically $ readQ q
      case mx of
        Nothing  -> collect next qs
        (Just x) -> do
          output x
          collect (q:next) qs
        
spread :: Task a b -> Int -> Task b c -> Task a c
spread t1 n t2 input output = do
  inQ  <- atomically $ newTQueue
  outQ <- atomically $ newTQueue
  feed inQ
    `concurrently_` jobs inQ outQ
    `concurrently_` collect outQ
  where
    feed inQ = do
      t1 input (atomically . writeTQueue inQ . Just)
      atomically $ writeTQueue inQ Nothing

    jobs inQ outQ = do
      replicateConcurrently_ n (job inQ outQ)
      atomically $ writeTQueue outQ Nothing

    job inQ outQ = t2 (atomically (readQ inQ)) (atomically . writeTQueue outQ . Just)

    collect outQ = forQ outQ output

withTask :: Task () b -> (IO (Maybe b) -> IO c) -> IO c
withTask task f = do
  outQ <- atomically $ newTQueue
  let job = task (return Nothing) (atomically . writeTQueue outQ . Just)
  withAsync job $ \_ -> f $ atomically $ readQ outQ
  
forInput :: IO (Maybe a) -> (a -> IO ()) -> IO ()
forInput input f = foldInput input () $ \_ x -> f x

foldInput :: IO (Maybe a) -> b -> (b -> a -> IO b) -> IO b
foldInput input acc f = do
  mx <- input
  case mx of
    Nothing  -> return acc
    (Just x) -> do
      acc' <- f acc x
      foldInput input acc' f

readQ :: TQueue (Maybe a) -> STM (Maybe a)
readQ q = do
  x <- peekTQueue q
  case x of
    Nothing -> return Nothing
    _       -> readTQueue q

forQ :: TQueue (Maybe a) -> (a -> IO ()) -> IO ()
forQ q f = do
  mx <- atomically $ readQ q
  case mx of
    Nothing  -> return ()
    (Just x) -> f x >> forQ q f
