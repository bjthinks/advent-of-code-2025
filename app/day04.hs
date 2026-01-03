module Main where

import Data.Array

numRollsNear :: Array (Int,Int) Char -> (Int,Int) -> Int
numRollsNear arr (x,y) = sum $ do
  x0 <- [x-1..x+1]
  y0 <- [y-1..y+1]
  let c = arr ! (x0,y0)
  return $ if c == '@' then 1 else 0

accessibleRolls :: Array (Int,Int) Char -> (Int,Int) -> [(Int,Int)]
accessibleRolls arr (width,height) = do
  x <- [1..width]
  y <- [1..height]
  let c = arr ! (x,y)
  case c of
    '@' -> if numRollsNear arr (x,y) <= 4 then [(x,y)] else []
    _ -> []

numAccessible :: Array (Int,Int) Char -> (Int,Int) -> Int
numAccessible arr (width,height) = length $ accessibleRolls arr (width,height)

removeRolls :: Array (Int,Int) Char -> [(Int,Int)] -> Array (Int,Int) Char
removeRolls arr rolls =
  let updates = map (\x -> (x,'.')) rolls
  in arr // updates

removeAllRolls :: Array (Int,Int) Char -> (Int,Int) -> Int
removeAllRolls arr (width,height) =
  case accessibleRolls arr (width,height) of
    [] -> 0
    rolls -> length rolls + removeAllRolls (removeRolls arr rolls) (width,height)

main :: IO ()
main = do
  raw <- lines <$> readFile "day04-input.txt"
  let width = length $ head raw
      height = length raw
      paper = zip [1..] $ map (zip [1..]) raw
      -- convert
      -- [(1,[(1,'a'),(2,'b')]),(2,[(1,'c'),(2,'d')])
      -- to
      -- [((1,1),'a'),((1,2),'b'),((2,1),'c'),((2,2),'d')]
      paper' = concat $ map (\(y,xs) -> map (\(x,c) -> ((y,x),c)) xs) paper
      arr = listArray ((0,0),(width+1,height+1)) (repeat '.') // paper'
  print $ numAccessible arr (width,height)
  print $ removeAllRolls arr (width,height)
