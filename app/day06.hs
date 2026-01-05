module Main where

import Data.List
import Data.List.Split

operator :: Char -> Int -> Int -> Int
operator '+' = (+)
operator '*' = (*)
operator _ = undefined

calculate :: [String] -> Int
calculate strings =
  let numbers = map read $ init strings
      op = operator $ head $ last strings
  in foldr1 op numbers

calculate' :: [String] -> Int
calculate' (s:ss) =
  let numbers = map read $ init s : ss
      op = operator $ last s
  in foldr1 op numbers
calculate' [] = undefined

main :: IO ()
main = do
  raw <- readFile "day06-input.txt"
  let input = map words $ lines raw
      transposedInput = transpose input
      answers = map calculate transposedInput
  print $ sum answers
  let input' = map (filter (/=' ')) $ transpose $ lines raw
      problems = splitOn [""] input'
      answers' = map calculate' problems
  print $ sum answers'
