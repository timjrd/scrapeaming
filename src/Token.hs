module Token (getToken) where

import qualified Data.ByteString as BS

import System.Entropy

tokenLength = 16

getToken :: IO String
getToken = map ((alphanum!!) . (`mod` n) . fromIntegral)
           <$> BS.unpack <$> getEntropy tokenLength
  where
    alphanum = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    n = length alphanum
