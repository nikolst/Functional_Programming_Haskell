main :: IO()
main = do
  print (adjPairs [1, 4, 7, 9, 6])

adjPairs :: [Int] -> [[Int]]
adjPairs [] = []
adjPairs xs
    | length xs == 2 = take 2 xs : []
    | otherwise = take 2 xs : (adjPairs (tail xs))
