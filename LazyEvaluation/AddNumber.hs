main :: IO()
main = do
  print (add_x 3 [1..])

add_x :: Int -> [Int] -> [Int]
add_x x xs = [x + y | y <- xs]
