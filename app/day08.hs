module Main where

import qualified Control.Monad.Union as UF
import Data.Array
import Data.Char
import Data.List

type Point = (Int, Int, Int)

parsePoint :: String -> Point
parsePoint str =
  let p1 = read $ takeWhile isDigit str
      str2 = tail $ dropWhile isDigit str
      p2 = read $ takeWhile isDigit str2
      str3 = tail $ dropWhile isDigit str2
      p3 = read str3
  in (p1, p2, p3)

two :: Int
two = 2

distance :: Point -> Point -> Int
distance (x1, y1, z1) (x2, y2, z2) =
  (x2 - x1)^two + (y2 - y1)^two + (z2 - z1)^two

m :: l -> l -> (l, ())
m x _ = (x, ())

joinManyNodes :: Array Int UF.Node -> Int -> [(Int, Int, Int)] -> UF.UnionM Int ()
joinManyNodes _ 0 _ = return ()
joinManyNodes nodeArray numWires ((_, x, y) : distances) = do
  _ <- UF.merge m (nodeArray ! x) (nodeArray ! y)
  joinManyNodes nodeArray (numWires - 1) distances
joinManyNodes _ _ _ = error "All nodes joined and more to go"

joinAndReportSizes :: Int -> Int -> [(Int, Int, Int)] -> UF.UnionM Int [Int]
joinAndReportSizes numNodes numWires distances = do
  nodes <- sequence $ map UF.new [1..numNodes]
  let nodeArray = listArray (1, numNodes) nodes
  joinManyNodes nodeArray numWires distances
  nodeReps <- sequence $ map UF.lookup nodes
  return $ reverse $ sort $ countRuns $ sort nodeReps

countRuns :: Eq a => [a] -> [Int]
countRuns input = c 0 input
  where
    c k (x:y:zs)
      | x == y = c (k+1) (y:zs)
      | otherwise = k+1 : c 0 (y:zs)
    c k [_] = [k + 1]
    c _ _ = undefined

main :: IO ()
main = do
  raw <- readFile "day08-input.txt"
  let pointStrs = lines raw
      pointList = map parsePoint pointStrs
      numNodes = length pointList
      points = listArray (1, numNodes) pointList
  let distances = [(distance (points ! x) (points ! y), x, y) |
                   x <- [1..numNodes], y <- [x+1..numNodes]]
      sortedDistances = sort distances
  print $ product $ take 3 $ UF.run $ joinAndReportSizes numNodes 1000 sortedDistances
