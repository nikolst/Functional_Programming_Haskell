main :: IO()
main = do
  print (withoutLastElem [2, 3, 8, 6, 6, 7, 2])
  
withoutLastElem :: [Int] -> [Int]
withoutLastElem xs = reverse (drop 1 (reverse xs))
