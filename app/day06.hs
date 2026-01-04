module Main where

import Data.List

calculate :: [String] -> Int
calculate strings =
  let numbers = map read $ init strings
      operator = case last strings of
        "+" -> (+)
        "*" -> (*)
        _ -> undefined
  in foldr1 operator numbers

main :: IO ()
main = do
  raw <- readFile "day06-input.txt"
  let input = map words $ lines raw
      transposedInput = transpose input
      answers = map calculate transposedInput
  print $ sum answers
