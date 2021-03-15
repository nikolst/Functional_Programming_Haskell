main :: IO()
main = do
    print (checkFunc 6)
    print (checkFunc 224)
    print (checkFunc' 2 6 8)
    print (checkFunc' 1 3 1)


-- checks if x is between 1 and 17 inclusive
checkFunc :: Integer -> Bool
checkFunc x 
    | x >= 1 && x <= 17 = True
    | otherwise         = False

--checks if a, b and c are different numbers
checkFunc' :: Integer -> Integer -> Integer -> Bool
checkFunc' a b c = if (a /= b && b /= c && a /= c) then True else False
