module Main where

getJoltage :: String -> Int
getJoltage str =
  let digits = map (read . \x -> [x]) str :: [Int]
      tensDigit = maximum $ init digits
      onesDigit = maximum $ tail $ dropWhile (<tensDigit) digits
  in tensDigit * 10 + onesDigit

main :: IO ()
main = do
  raw <- readFile "day03-input.txt"
  let joltages = map getJoltage $ lines raw
  print $ sum joltages
