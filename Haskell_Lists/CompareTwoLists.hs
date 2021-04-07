main :: IO()
main = do
    print (comp [1, 2, 3] [1, 2, 3])
    print (comp [1, 4, 3] [1, 2, 12])
    print (comp [1, 2, 3, 7, 8] [1, 2, 3])

comp :: [Int] -> [Int] -> Bool
comp xs ys 
    | xs == [] && ys == []  = True
    | xs == [] || ys == []  = False
    | otherwise             = (head xs == head ys) && (tail xs == tail ys)
