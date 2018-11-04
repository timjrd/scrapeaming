module Process where

import Control.Exception

import System.IO
import System.Process
import System.Timeout

data Stream = Stdout | Stderr

withProcess :: FilePath -> [String] -> Stream -> Int -> a
            -> (a -> String -> IO (Maybe a)) -> IO ()
withProcess path args stream time init consume = bracket acquire release use
  where
    acquire :: IO (Handle, ProcessHandle)
    acquire = case stream of
      Stdout -> cr pr {std_out = CreatePipe} >>= \(_, Just h, _, p) -> return (h,p)
      Stderr -> cr pr {std_err = CreatePipe} >>= \(_, _, Just h, p) -> return (h,p)
      where
        cr = createProcess
        pr = proc path args

    release (h,p) = hClose h >> terminateProcess p

    use (h,_) = timeout (time*10^6) (f h $ Just init) >> return ()
      where
        f _ Nothing  = return Nothing
        f h (Just x) = do
          eof <- hIsEOF h
          if eof
            then return Nothing
            else hGetLine h
                 >>= consume x
                 >>= f h

