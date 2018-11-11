module Cli (main) where

import Control.Exception

import Data.List

import System.Environment
import System.IO
import System.Console.ANSI

import Driver

main :: IO ()
main = uninterruptibleMask $ \restore ->
  (restore search `catch` interrupted (True,[]))
  >>= printDone >>= printResults

search :: IO (Bool,[Result])
search = do
  [query] <- getArgs
  printStatus []
  withDriver
    $ \driver  -> driver query
    $ \input   -> foldInputCatch interrupted input (True,[]) processInput

processInput :: (Bool,[Result]) -> Either Log Result -> IO (Bool,[Result])
processInput (_,xs) (Left  l) = printLog l >> printStatus xs >> return (True,xs)
processInput (_,xs) (Right x) = printStatus ys >> return (True,ys)
  where ys | x `elem` xs = xs
           | otherwise   = sortBy (flip compare) $ x:xs

interrupted :: (Bool,a) -> AsyncException -> IO (Bool,a)
interrupted (_,x) UserInterrupt = return (False,x)
interrupted _ e = throw e

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
  green "1 video found: "
  printShortResult x
  space
  
printStatus xs  = do
  clear
  searching
  green $ show (length xs) ++ " videos found. "
  def "best: "
  printShortResult $ head xs
  space

searching = def "searching... "

printDone (True , x) = clear >> green "search over." >> return x
printDone (False, x) = clear >> red   "interrupted." >> return x

printResults [] = ret >> red "no videos found." >> ret
printResults xs = do
  ret
  green $ show (length xs) ++ " video" ++ s ++ " found:"
  ret >> ret
  mapM_ printResult $ reverse $ zip [1..] xs
  where
    s = if length xs > 1 then "s" else ""

printResult (i,x) = do
  def $ "#" ++ show i
  space
  printShortResult x >> ret
  printTrace def (sources x)

printShortResult x = do
  printDuration (duration x)
  space
  printQuality  (quality  x)
  space
  def $ show $ width x
  def "x"
  def $ show $ height x
  space
  def $ short $ title x

printDuration x
  | x < 60      = red    $ seconds x
  | x < 60 * 5  = red    $ minutes x
  | x < 60 * 30 = yellow $ minutes x
  | x < 60 * 60 = green  $ minutes x
  | otherwise   = green (hours x) >> space >> green (minutes x)
    
seconds x = show (x `mod` 60) ++ "s"
minutes x = show (x `mod` (60*60) `div` 60) ++ "min"
hours   x = show (x `div` (60*60)) ++ "h"

printQuality x
  | x < 15    = red    $ show x ++ "%"
  | x < 50    = yellow $ show x ++ "%"
  | otherwise = green  $ show x ++ "%"

printTrace col = col . (++"\n") . unlines . map ("    "++)

short x
  | length x > n = take (n-2) x ++ ".."
  | otherwise    = x
  where n = 30

space  = def " "
clear  = clearLine >> setCursorColumn 0
ret    = def "\n"

def x  = putStr x >> hFlush stdout
green  = withSGR [SetColor Foreground Vivid Green]
yellow = withSGR [SetColor Foreground Vivid Yellow]
red    = withSGR [SetColor Foreground Vivid Red]

withSGR sgr str = setSGR sgr >> def str >> setSGR [Reset]
