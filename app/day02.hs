module Main where

parseRanges :: String -> [String]
parseRanges str =
  let (x,y) = break (==',') str
  in case y of
    "" -> [x]
    ys -> x : parseRanges (tail ys)

parseRange :: String -> (Int,Int)
parseRange str =
  let (begin,end) = break (=='-') str
  in (read begin,read (tail end))

parse :: String -> [(Int,Int)]
parse str = map parseRange $ parseRanges str

isInvalid :: Int -> Bool
isInvalid x =
  let xStr = show x
      len = length xStr
      halfLen = len `div` 2
      (y,z) = splitAt halfLen xStr
  in y == z

invalidInRange :: (Int,Int) -> [Int]
invalidInRange (x,y)
  | x > y = []
  | isInvalid x = x : invalidInRange (x+1,y)
  | otherwise = invalidInRange (x+1,y)

isGenInvalid :: Int -> Bool
isGenInvalid x =
  let xStr = show x
      len = length xStr
      substrLengths = [1..len-1]
  in or $ map (isGenInvalid' xStr) substrLengths
  where
    isGenInvalid' xStr partLen = allEqual $ partsOfLen partLen xStr
    partsOfLen tryLen str
      | length str <= tryLen = [str]
      | otherwise = let (y,z) = splitAt tryLen str
                    in y : partsOfLen tryLen z
    allEqual [] = True
    allEqual [_] = True
    allEqual (w:ws)
      | w == head ws = allEqual ws
      | otherwise = False

genInvalidInRange :: (Int,Int) -> [Int]
genInvalidInRange (x,y)
  | x > y = []
  | isGenInvalid x = x : genInvalidInRange (x+1,y)
  | otherwise = genInvalidInRange (x+1,y)

main :: IO ()
main = do
  input <- readFile "day02-input.txt"
  let ranges = parse input
  print $ sum $ concat $ map invalidInRange ranges
  print $ sum $ concat $ map genInvalidInRange ranges
