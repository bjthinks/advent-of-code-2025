module Main where

parseRange :: String -> (Int, Int)
parseRange str = (read (takeWhile (/='-') str),
                  read (tail (dropWhile (/='-') str)))

withinARange :: [(Int, Int)] -> Int -> Bool
withinARange [] _ = False
withinARange ((lo,hi):rs) value
  | value >= lo && value <= hi = True
  | otherwise = withinARange rs value

main :: IO ()
main = do
  raw <- readFile "day05-input.txt"
  let rawLines = lines raw
      rawRanges = takeWhile (/="") rawLines
      rawIDs = tail $ dropWhile (/="") rawLines
      ranges = map parseRange rawRanges
      ids = map read rawIDs
  print $ length $ filter (withinARange ranges) ids
