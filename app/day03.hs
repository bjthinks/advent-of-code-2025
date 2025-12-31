module Main where

getJoltage :: Int -> String -> Int
getJoltage numDigits str = read $ concat $ map show $
  getJoltage' numDigits $ map (read . \x -> [x]) str

getJoltage' :: Int -> [Int] -> [Int]
getJoltage' numDigits digits =
  let len = length digits
      nextDigit = maximum $ take (len - numDigits + 1) digits
      remainingString = tail $ dropWhile (<nextDigit) digits
  in case numDigits of
    0 -> []
    _ -> nextDigit : getJoltage' (numDigits-1) remainingString

main :: IO ()
main = do
  raw <- readFile "day03-input.txt"
  let joltages2 = map (getJoltage 2) (lines raw)
  print $ sum joltages2
  let joltages12 = map (getJoltage 12) (lines raw)
  print $ sum joltages12
