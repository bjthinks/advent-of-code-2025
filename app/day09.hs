module Main where

type Tile = (Int, Int)

parseTile :: String -> Tile
parseTile s =
  let x = read $ takeWhile (/=',') s
      y = read $ tail $ dropWhile (/=',') s
  in (x, y)

calculateAreaAllPairs :: [Tile] -> [Int]
calculateAreaAllPairs tiles = do
  x <- tiles
  y <- tiles
  return $ abs (fst x - fst y + 1) * abs (snd x - snd y + 1)

main :: IO ()
main = do
  raw <- readFile "day09-input.txt"
  let tileStrs = lines raw
      tiles = map parseTile tileStrs
      areas = calculateAreaAllPairs tiles
  print $ maximum areas
