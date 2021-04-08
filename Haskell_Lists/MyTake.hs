main :: IO()
main = do
    print (my_take 3 [2, 5, 6, 7, 8])
    print (my_take 0 [2, 5, 8, 9, 12])
    
my_take :: Int -> [Int] -> [Int]
my_take 0 _  = []
my_take n xs = head xs : my_take (n - 1) (tail xs)
