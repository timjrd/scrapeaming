module Cli (main) where

import Control.Exception

import Data.IORef

import System.IO
import System.Console.ANSI

import Driver

main :: String -> IO ()
main query = uninterruptibleMask $ \restore -> do
  results <- newIORef []
  xe      <- try $ restore $ search query results
  case xe of
    Right x -> printCompleted >> printResults x
    Left  e -> do
      printInterrupted
      readIORef results >>= printResults
      case e of
        UserInterrupt -> return ()
        _             -> throw e

search :: String -> IORef [Result] -> IO [Result]
search query results = do
  uninterruptibleMask_ $ printStatus []
  withDriver
    $ \driver -> driver query
    $ \input  -> foldInput input []
    $ \xs lx  -> uninterruptibleMask_ $ case lx of
    Left  l   -> printLog l >> printStatus xs >> return xs
    Right x   -> do
      let ys = addResult x xs
      writeIORef results ys
      printStatus ys
      return ys

printLog (Log tag msg trace) = do
  clear
  col msg >> ret
  printTrace col trace
  where
    col = case tag of
      Important -> green
      Info      -> def
      Warning   -> yellow
      Error     -> red

printStatus [] = do
  clear
  searching
  yellow "no videos found yet."
  space
  
printStatus [x] = do
  clear
  searching
  green "1 video found:"
  space
  printShortResult x
  space
  
printStatus xs  = do
  clear
  searching
  green $ show (length xs) ++ " videos found."
  space
  def "best: "
  printShortResult $ head xs
  space

searching = def "searching... "

printCompleted   = clear >> green  "search completed."
printInterrupted = clear >> yellow "search interrupted."

printResults [] = ret >> yellow "no videos found." >> ret
printResults xs = do
  ret
  green $ show (length xs) ++ " video" ++ s ++ " found:"
  ret >> ret
  mapM_ printResult $ reverse $ zip [1..] xs
  where
    s = if length xs > 1 then "s" else ""

printResult (i,x) = do
  bold $ "#" ++ show i
  space
  printShortResult x
  ret
  printTrace def (sources x)

printShortResult x = do
  printDuration (duration x)
  space
  printQuality  (quality  x)
  space
  bold $ show $ width x
  bold "x"
  bold $ show $ height x
  space
  def $ "\"" ++ short (title x) ++ "\""

printDuration x
  | x < 60      = red    $ seconds x
  | x < 60 * 5  = red    $ minutes x
  | x < 60 * 30 = yellow $ minutes x
  | x < 60 * 60 = green  $ minutes x
  | otherwise   = green  $ hours x ++ " " ++ minutes x
    
seconds x = show (x `mod` 60) ++ "s"
minutes x = show (x `mod` (60*60) `div` 60) ++ "min"
hours   x = show (x `div` (60*60)) ++ "h"

printQuality x
  | x < 15    = red    $ show x ++ "%"
  | x < 50    = yellow $ show x ++ "%"
  | otherwise = green  $ show x ++ "%"

printTrace col xs = mapM_ f xs >> ret
  where f x = space >> space >> col x >> ret

short = take 30

space  = def " "
clear  = clearLine >> setCursorColumn 0
ret    = def "\n"

def x  = putStr x >> hFlush stdout
green  = colored Green
yellow = colored Yellow
red    = colored Red
bold   = withSGR [SetConsoleIntensity BoldIntensity]

colored color = withSGR [SetColor Foreground Dull color]

withSGR sgr str = setSGR sgr >> def str >> setSGR [Reset]
