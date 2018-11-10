module Display
  (withDisplay) where

import System.IO

import Environment
import Process

withDisplay :: (Int -> IO a) -> IO a
withDisplay f = do
  xvfb <- getXvfb
  withProcess xvfb ["-displayfd", "1"] $ \_ hout _ ->
    hGetLine hout >>= f . read
