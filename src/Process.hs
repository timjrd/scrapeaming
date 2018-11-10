module Process
  ( withProcess
  , hFold
  , hGetLines ) where

import Control.Monad
import Control.Exception

import System.IO
import System.Process

bracket' acquire release use = uninterruptibleMask $ \restore ->
  bracket acquire release $ restore . use

withProcess :: FilePath -> [String]
            -> (Handle -> Handle -> Handle -> IO a) -> IO a
withProcess path args f = bracket' acquire release use
  where
    acquire = createProcess (proc path args)
              { std_in  = CreatePipe
              , std_out = CreatePipe
              , std_err = CreatePipe }
              >>= \(Just hin, Just hout, Just herr, hp) ->
                    return (hin,hout,herr,hp)

    release (hin,hout,herr,hp) = do
      hClose hin
      hClose hout
      hClose herr
      terminateProcess hp

    use (hin,hout,herr,_) = f hin hout herr

hFold :: Handle -> a -> (a -> String -> IO (Bool,a)) -> IO a
hFold h init f = g (True,init)
  where
    g (False, x) = return x
    g (True , x) = do
      eof <- hIsEOF h
      if eof
        then return x
        else hGetLine h
             >>= f x
             >>= g

hGetLines :: Handle -> IO [String]
hGetLines h = fmap reverse $ hFold h [] $ \lines line -> return (True,line:lines)
