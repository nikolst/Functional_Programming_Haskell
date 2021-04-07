main :: IO()
main = do
    print [0..19]
    print [1, 3 .. 40]
    print [2, 4 .. 40]
    print (reverse(power 5))
    
power :: Int -> [Int]
power 0 = [1]
power 1 = [2]
power n = 2 ^ n : power (n - 1)
