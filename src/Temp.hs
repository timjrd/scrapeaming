module Temp where

import qualified System.IO.Temp as TMP

import Control.Exception

withSystemTempDirectory' name f = uninterruptibleMask $ \restore ->
  TMP.withSystemTempDirectory name $ restore . f
