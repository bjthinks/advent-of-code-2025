module Main where

import Control.Monad.State

left :: Int -> State Int ()
left n = do
  x <- get
  let y = (x - n) `mod` 100
  put y

right :: Int -> State Int ()
right n = do
  x <- get
  let y = (x + n) `mod` 100
  put y

parseMove :: String -> State Int Int
parseMove (dir:val)
  | dir == 'L' = left (read val) >> get
  | dir == 'R' = right (read val) >> get
  | otherwise = undefined
parseMove [] = undefined

leftClick :: Int -> State Int Int
leftClick n = do
  x <- get
  let y = (x - n) `mod` 100
  put y
  let adjust = (if x == 0 then (-1) else 0) + (if y == 0 then 1 else 0)
  return $ abs ((x - n) `div` 100) + adjust

rightClick :: Int -> State Int Int
rightClick n = do
  x <- get
  let y = (x + n) `mod` 100
  put y
  return $ (x + n) `div` 100

parseMoveClick :: String -> State Int Int
parseMoveClick (dir:val)
  | dir == 'L' = leftClick (read val)
  | dir == 'R' = rightClick (read val)
  | otherwise = undefined
parseMoveClick [] = undefined

main :: IO ()
main = do
  raw <- readFile "day01-input.txt"
  let commands = lines raw
      moves = map parseMove commands
      positions = evalState (sequence moves) 50
  print $ length $ filter (==0) positions
  let clicks = map parseMoveClick commands
      zeroesList = evalState (sequence clicks) 50
  print $ sum zeroesList
