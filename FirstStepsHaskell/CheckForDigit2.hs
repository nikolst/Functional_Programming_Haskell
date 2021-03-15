main :: IO()
main = do
    print (checkDigit 334)
    print (checkDigit 526)
    print (checkDigit 794231)
    print (checkDigit 679013465)


checkDigit :: Integer -> Bool
checkDigit x
    | x == 0 = False
    | (x `mod` 10) == 2 = True
    | otherwise = checkDigit (x `div` 10)
