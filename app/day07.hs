module Main where

processPair :: (String, Int) -> String -> (String, Int)
processPair (above, countAbove) current = (init result, countAbove + countThis)
  where (result, countThis) =
          processPair' ("." ++ above ++ ".") ("." ++ current ++ ".")

processPair' :: String -> String -> (String, Int)
processPair' above current
  | length above < 3 = (".", 0)
  | above !! 1 == 'S' = ('|' : rest, restCount)
  | above !! 1 == '|' && current !! 1 == '.' = ('|' : rest, restCount)
  | above !! 1 == '|' && current !! 1 == '^' = ('^' : rest, restCount + 1)
  | above !! 1 == '.' && current !! 1 == '^' = ('^' : rest, restCount)
  | above !! 0 == '|' && current !! 0 == '^' = ('|' : rest, restCount)
  | above !! 2 == '|' && current !! 2 == '^' = ('|' : rest, restCount)
  | above !! 1 == '.' && current !! 1 == '.' = ('.' : rest, restCount)
  | above !! 1 == '^' && current !! 1 == '.' = ('.' : rest, restCount)
  | otherwise = undefined
  where
    (rest, restCount) = processPair' (tail above) (tail current)

processQuantum :: [(Char, Int)] -> String -> [(Char, Int)]
processQuantum above current = init $
  processQuantum' ([('.', 0)] ++ above ++ [('.', 0)]) ("." ++ current ++ ".")

processQuantum' :: [(Char, Int)] -> String -> [(Char, Int)]
processQuantum' ((a0, a0c) : ta@((a1, a1c) : (a2, a2c) : _)) current
  | a1 == 'S' = ('|',1) : rest
  | a1 == '|' && current !! 1 == '^' = ('^', 0) : rest
  | a1 == '.' && current !! 1 == '^' = ('^', 0) : rest
  | a0 == '|' && a1 == '.' && a2 == '|' && current !! 0 == '^' && current !! 2 == '^' = ('|', a0c + a2c) : rest
  | a0 == '|' && a1 == '|' && a2 == '|' && current !! 0 == '^' && current !! 2 == '^' = ('|', a0c + a1c + a2c) : rest
  | a0 == '|' && a1 == '.' && current !! 0 == '^' = ('|', a0c) : rest
  | a0 == '|' && a1 == '|' && current !! 0 == '^' = ('|', a0c + a1c) : rest
  | a2 == '|' && a1 == '.' && current !! 2 == '^' = ('|', a2c) : rest
  | a2 == '|' && a1 == '|' && current !! 2 == '^' = ('|', a1c + a2c) : rest
  | a1 == '|' && current !! 1 == '.' = ('|', a1c) : rest
  | a1 == '.' && current !! 1 == '.' = ('.', 0) : rest
  | a1 == '^' && current !! 1 == '.' = ('.', 0) : rest
  | otherwise = undefined
  where
    rest = processQuantum' ta (tail current)
processQuantum' [_, _] _ = [('.', 0)]
processQuantum' _ _ = undefined

main :: IO ()
main = do
  raw <- readFile "day07-input.txt"
  let rows = lines raw
  print $ snd $ foldl processPair (head rows, 0) (tail rows)
  let firstRowWithCounts = map (\c -> (c, 0)) (head rows)
  print $ sum $ map snd $ foldl processQuantum firstRowWithCounts (tail rows)
