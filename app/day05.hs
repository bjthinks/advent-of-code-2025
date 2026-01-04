module Main where

import Data.List

parseRange :: String -> (Int, Int)
parseRange str = (read (takeWhile (/='-') str),
                  read (tail (dropWhile (/='-') str)))

withinARange :: [(Int, Int)] -> Int -> Bool
withinARange [] _ = False
withinARange ((lo,hi):rs) value
  | value >= lo && value <= hi = True
  | otherwise = withinARange rs value

combineRanges :: [(Int, Int)] -> [(Int, Int)]
combineRanges [] = []
combineRanges [x] = [x]
combineRanges ((lo1,hi1):(lo2,hi2):rs)
  | lo2 <= hi1 + 1 = combineRanges ((lo1,max hi1 hi2):rs)
  | otherwise = (lo1,hi1) : combineRanges ((lo2,hi2):rs)

rangeSize :: (Int, Int) -> Int
rangeSize (lo,hi) = hi - lo + 1

main :: IO ()
main = do
  raw <- readFile "day05-input.txt"
  let rawLines = lines raw
      rawRanges = takeWhile (/="") rawLines
      rawIDs = tail $ dropWhile (/="") rawLines
      ranges = map parseRange rawRanges
      ids = map read rawIDs
  print $ length $ filter (withinARange ranges) ids
  print $ sum $ map rangeSize $ combineRanges $ sort ranges
