main :: IO()
main = do
    print (lastEelem [1, 4, 77, 8, 19])
    print (lastEelem [0])
    print (lastEelem [])

lastEelem :: [Int] -> Int
lastEelem xs 
    | xs == []  = error "The list is empty!"
    | otherwise = head (reverse xs)
