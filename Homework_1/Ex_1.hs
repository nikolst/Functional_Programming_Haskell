main :: IO()
main = do
    print (isPrime 7)
    print (isPrime 9)
    print (isPrime 8)
    print (isPrime 2047)

    print (safePrimesCount 20 100)
    print (safePrimesCount 1 983)
    print (safePrimesCount 167 1892)
    print (safePrimesCount 1678 20097)

    print (isSpecial 31)
    print (isSpecial 127)
    print (isSpecial 2047)


-- Задача 1 а) 
-- с функцията isPrime проверяваме дали числото е просто, т.е. се дели само на 1 и на себе си
isPrime :: Integer -> Bool
isPrime x 
    | x == 1    = False
    | otherwise = helper 2
    where 
        helper n
            | n == x         = True
            | x `mod` n == 0 = False
            | otherwise      = helper (n + 1)
            
--а и b са началната и крайната стойност на интервала. 
safePrimesCount :: Integer -> Integer -> Integer
safePrimesCount a b = helper a 0
    where 
        helper x count
            | x > b                                 = count
            | isPrime x && isPrime((x - 1) `div` 2) = helper (x + 1) (count + 1)
            | otherwise                             = helper (x + 1) count


-- Задача 1 б) 
--правим проверка дали числото е "особено"
isSpecial :: Integer -> Bool
isSpecial x 
    | isPrime x  = helper (x+1) 1
    | otherwise  = False
    where 
        helper k counter
            | k == 2 && (counter `mod` 2 /= 0) = True
            | k `mod` 2 == 0                   = helper (k `div` 2) (counter + 1)
            | otherwise                        = False
   
-- сумираме "особените" числа 
specialSum :: Integer -> Integer -> Integer
specialSum k m = helper (m+1) k 0
    where 
        helper x i sum
            | isSpecial x && i == 0 = sum
            | isSpecial x           = helper (x+1) (i-1) (sum + x) 
            | otherwise             = helper (x+1) i sum
