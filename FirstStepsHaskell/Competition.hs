main :: IO()
main = do
    print (ranking 3)
    print (ranking 6)

-- r is the place of the competitor
ranking :: Integer -> Integer
ranking r
    | r == 1    = 10
    | r == 2    = 7
    | r == 3    = 5
    | r == 4    = 4
    | r == 5    = 3
    | r == 6    = 2
    | r == 7    = 1
    | otherwise = 0 
