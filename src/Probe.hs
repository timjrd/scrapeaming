module Probe
  ( Result
  , probe
  , sources
  , quality
  , duration
  , width
  , height
  , title ) where

import Data.Function ((&))
import Data.Maybe
import Data.Ratio
import Data.List
import Numeric

import Network.URI

import Environment
import Task
import Process
import Logger

type Url = String

data Result = Result
  { sources  :: [Url]
  , quality  :: Int -- between 0 and 100
  , duration :: Int -- seconds
  , width    :: Int
  , height   :: Int
  , title    :: String }
  deriving Show

data RawResult = NoParse | RawResult
  { rawWidth    :: Rational
  , rawHeight   :: Rational
  , rawDuration :: Rational
  , rawBitrate  :: Rational
  , rawTitle    :: String }
  deriving Show

instance Eq Result where
  a == b = head (sources a) == head (sources b)

instance Ord Result where
  compare a b
    | a == b               = EQ
    | max (d a) (d b) <  t = compare (q a, d a) (q b, d b)
    | abs (d a - d b) >= t = compare (d a) (d b)
    | otherwise            = compare (q a, d a) (q b, d b)
    where
      d = duration
      q = quality
      t = 60 * 10

probe :: Logger -> Task [Url] Result
probe log input output = forInput input $ \srcs -> do
  raw <- rawProbe $ head srcs
  case raw of
    Nothing      -> log $ Log Warning "unable to probe"                  srcs
    Just NoParse -> log $ Log Error   "unable to parse ffprobe's output" srcs
    Just res     -> do
      output $ toResult srcs res
      log $ Log Important "found suitable result" srcs

rawProbe :: Url -> IO (Maybe RawResult)
rawProbe url = do
  ffprobe <- getFFprobe
  withProcess ffprobe
    [ "-print_format"  , "default=noprint_wrappers=1:nokey=1"
    , "-select_streams", "v:0"
    , "-show_entries"  , "stream=width,height:format=duration,bit_rate:format_tags=title"
    , url ]
    $ \_ hout _ -> do
    result <- hGetLines hout
    return $ case result of
      mw:mh:md:mb:mt ->
        maybe (Just NoParse) Just $ do
        w <- readRational mw
        h <- readRational mh
        d <- readRational md
        b <- readRational mb
        return RawResult
          { rawWidth    = max 1 w
          , rawHeight   = max 1 h
          , rawDuration = max 0 d
          , rawBitrate  = max 0 b
          , rawTitle    = concat mt }
      [] -> Nothing
      _  -> Just NoParse

toResult :: [Url] -> RawResult -> Result
toResult srcs raw = Result
  { sources  = srcs
  , quality  = floor $ 100 * relQuality (absQuality raw)
  , duration = floor $ rawDuration raw
  , width    = floor $ rawWidth    raw
  , height   = floor $ rawHeight   raw
  , title    = fromMaybe fallbackTitle $ notNull $ rawTitle raw }
  where
    fallbackTitle = 
      parseURI (head srcs)
      <&> pathSegments
      >>= notNull
      <&> last
      <&> unEscapeString
      >>= notNull
      &   fromMaybe (head srcs)

notNull :: [a] -> Maybe [a]
notNull [] = Nothing
notNull x  = Just x

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- Maximum DVD resolution.
loWidth  = 720
loHeight = 576

-- Computed from samples.
loQuality = (1197773 % 737280, 0.3)
hiQuality = (2489777 % 460800, 0.9)

relQuality :: Rational -> Rational
relQuality x = max 0 $ min 1 $ d + k * x
  where
    d = y1 - k * x1
    k = (y2 - y1) / (x2 - x1)
    (x1,y1) = loQuality
    (x2,y2) = hiQuality
    
absQuality :: RawResult -> Rational
absQuality raw = rb * rw * rh
  where
    rb = rawBitrate raw / (rawWidth raw * rawHeight raw)
    rw = min (rawWidth  raw) loWidth  / loWidth
    rh = min (rawHeight raw) loHeight / loHeight

readRational :: String -> Maybe Rational
readRational = fmap (fst . fst) . uncons . readFloat

