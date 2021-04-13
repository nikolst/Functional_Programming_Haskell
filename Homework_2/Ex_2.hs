main :: IO()
main = do
    --print (takeFirst 3 [1, 4, 8, 0, 12, 6, 7]) -- test help function
    --print (takeSecond 3 [1, 4, 8, 0, 12, 6, 7]) -- test help function
    --print (twoHalfs 3 [1, 4, 8, 0, 12, 6, 7]) -- test help function

    
    print ((josephus [1, 2, 3, 4, 5, 6, 7]) 3)
    print ((josephus [1,2,3,4,5,6,7,8,9,10]) 1)
    print ((josephus [1,2,3,4,5,6,7,8,9,10]) 2)
    print ((josephus "fpFMIsu") 4)
    --print ((josephus [1, 2, 3, 4, 5, 6, 7]) (-1))

-- take the first n elements of a list and remove the n-th element
takeFirst :: Int -> [a] -> [a]
takeFirst n [] = []
takeFirst n xs = reverse (drop 1 (reverse (take n xs)))

-- take the elements after the n-th element from a list
takeSecond :: Int -> [a] -> [a]
takeSecond n [] = []
takeSecond n xs = drop n xs

-- concatenate two halfs without n-th element from second to first
twoHalfs :: Int -> [a] -> [a]
twoHalfs n [] = []
twoHalfs n xs = (takeSecond n xs) ++ (takeFirst n xs)

josephus  :: Eq a => [a] -> (Int -> [a])
josephus xs = func
    where 
        func n = if (n > 0) then helper xs n [] else error "k was not natural"
            where 
                helper xs n zs 
                    | xs == [] = reverse zs
                    | length xs < n  = helper (drop 1 xs) n (head xs : zs)
                    | otherwise      = helper (twoHalfs n xs) n ((xs !! (n-1)) : zs)
