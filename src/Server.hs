{-# LANGUAGE DeriveDataTypeable #-}
module Server (main) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async hiding (cancel)
import Control.Exception

import Data.List
import qualified Data.Map.Strict  as M
import Data.ByteString.Char8      as B (pack)
import Data.ByteString.Lazy.Char8 as L (pack)

import Text.JSON.Generic hiding (Result)

import System.FilePath

import Happstack.Server

import Environment
import Driver
import Token
import Logger

type Pool = TVar (M.Map Token (Status, IO ()))

data Status = Status
  { results :: [Result]
  , logs    :: [String]
  , done    :: Bool }
  deriving (Data)

instance ToMessage Status where
  toContentType _ = B.pack "application/json"
  toMessage       = L.pack . encodeJSON

maxLogs = 10

emptyStatus = Status [] [] False

main :: Int -> IO ()
main port = uninterruptibleMask $ \restore -> do
  pool <- atomically $ newTVar M.empty
  me   <- try $ restore $ serve port pool
  putStrLn "canceling all jobs before shutdown..."
  atomically (readTVar pool) >>= mapM_ snd
  putStrLn "done."
  case me of
    Right ()           -> return ()
    Left UserInterrupt -> return ()
    Left e             -> throw e
    
serve :: Int -> Pool -> IO ()
serve port pool = do
  (log,logs) <- newLogger
  withAsync (logPrinter logs) $ \_ -> withDriver $ \driver ->
    simpleHTTP nullConf { port = port, logAccess = Nothing } $ msum
    [ search pool driver log
    , status pool
    , cancel pool
    , static ]

search :: Pool -> Driver () -> Logger -> ServerPart Response
search pool driver log = dir "search" $ path $ \query ->
  lift (newJob pool driver log query) >>= ok . toResponse

status :: Pool -> ServerPart Response
status pool = dir "status" $ path $ \token -> do
  ms <- lift $ getStatus pool token
  case ms of
    Just status -> ok $ toResponse $ status
    Nothing     -> jobNotFound token

cancel :: Pool -> ServerPart Response
cancel pool = dir "cancel" $ path $ \token -> do
  found <- lift $ cancelJob pool token
  if found
    then ok $ toResponse ()
    else jobNotFound token

jobNotFound :: Token -> ServerPart Response
jobNotFound token = notFound $ toResponse $ "Job " ++ token ++ " not found." 

static :: ServerPart Response
static = lift getRoot >>=
  serveDirectory EnableBrowsing ["index.html"] . (</> "static")

newJob :: Pool -> Driver () -> Logger -> String -> IO Token
newJob pool driver log query = do
  token <- getToken
  uninterruptibleMask $ \restore -> do
    j <- async $ restore $ job token
    atomically $ modifyTVar' pool $
      M.insert token (emptyStatus, uninterruptibleCancel j)
  return token
  where
    job token = driver query $ \input -> do
      result <- foldInput input emptyStatus f
      setStatus result { done = True }
      where
        f xs (Left l@(Log tag msg trace)) = do
          log $ Log tag msg $ trace ++ [token]
          let ys = xs { logs = take maxLogs $ logStr l : logs xs }
          setStatus ys
          return ys
          
        f xs (Right x) = do
          let ys = xs { results = addResult x $ results xs }
          setStatus ys
          return ys
        
        setStatus x = atomically $ modifyTVar' pool $
          M.adjust (\(_,stop) -> (x,stop)) token

getStatus :: Pool -> Token -> IO (Maybe Status)
getStatus pool token = atomically (readTVar pool) >>= return . fmap fst . M.lookup token

cancelJob :: Pool -> Token -> IO Bool
cancelJob pool token = do
  jobs <- atomically $ readTVar pool
  case M.lookup token jobs of
    Nothing       -> return False
    Just (_,stop) -> stop >> return True

logStr :: Log -> String
logStr (Log _ msg trace) = intercalate " @ " $ msg : trace

logPrinter :: IO Log -> IO ()
logPrinter nextLog = forever $ nextLog >>= putStrLn . logStr
