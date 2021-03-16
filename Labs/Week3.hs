main :: IO()
main = do

    print (pow 2 3)
    print (pow 4 5)

    print (isPrime 23)
    print (isPrime 14)
    print (isPrime 1)
    print (isPrime 0)

    print (isAscending 123456)

    print (isAscending' 1234)
    print (isAscending' 76543)
    print (isAscending' 1749)
    print (isAscending' 1479)

    print (countOccurences 3 735432)
    print (countOccurences 1 87490)

    print (isPerfectNumber 6)
    print (isPerfectNumber 3)
    print (isPerfectNumber 28)

    print (sumPrimeDivisors 6)
    print (sumPrimeDivisors 15)
    print (sumPrimeDivisors 21)


{-
    Задача 1. 
    Да се дефинира функция pow, която генерира линейно рекурсивен процес и намира x на степен n, 
    където x е реално, а n - естествено число.
-}

pow :: Integer -> Integer -> Integer
pow x n 
    | x == 0 = 0
    | x == 1 || n == 0 = 1
    | otherwise = x * (pow x (n - 1))

{-
    Задача 2. 
    Да се дефинира предикат isPrime, който проверява дали дадено естествено число е просто.
    (Заб.: Числото 1 не е нито просто, нито съставно.)
-}

isPrime :: Integer -> Bool
isPrime x = if (x `mod` 2 /= 0 && x /= 1) then True else False

{-
    Задача 3.
    Да се дефинира предикат isAscending, който връща истина, ако цифрите на дадено естествено 
    число са в нарастващ ред от първата към последната.
-}
-- 1st way
isAscending :: Integer -> Bool
isAscending n 
    | n < 10    = True
    | otherwise =  helper (n `div` 10)(n `mod` 10)
    where  
        helper num lastDigit
            | num < 10                    = num < lastDigit
            | (num `mod` 10) >= lastDigit = False
            | otherwise                   = helper (num `div` 10)(num `mod` 10)

-- 2nd way
isAscending' :: Integer -> Bool
isAscending' n = n < 10 || ((n `div` 10) `mod` 10 < (n `mod` 10) && isAscending' (n `div` 10))

{-
    Задача 4. 
    Да се дефинира функция countOccurences, намираща броя на срещанията на дадена цифра d 
    в записа на число n.
-}

countOccurences :: Integer -> Integer -> Integer
countOccurences d n 
    | n < 10 = if (n == d) then 1 else 0
    | otherwise = helper n 0
        where 
            helper x count
                | x == 0          = count
                | x `mod` 10 == d = helper (x `div` 10) (count + 1)
                | otherwise       = helper (x `div` 10) count

{-
    Задача 5. 
    Да се дефинира предикат isPerfectNumber, който връща дали едно число е съвършено, 
    т.е. равно на сумата от делителите му.
-}

isPerfectNumber :: Integer -> Bool
isPerfectNumber x = x == sumDivisors x

sumDivisors :: Integer -> Integer
sumDivisors n = helper 1 0
    where 
        helper del sum
            | del == n         = sum
            | n `mod` del == 0 = helper (del + 1) (sum + del)
            | otherwise        = helper (del + 1) sum

{-
    Задача 6. 
    Да се дефинира функция sumPrimeDivisors, която намира сумата на всички прости делители на едно число.
-}

sumPrimeDivisors :: Integer -> Integer
sumPrimeDivisors n = helper 1
    where 
        helper primeDel
            | n == primeDel                        = 0
            | n `mod` primeDel == 0 && isPrime primeDel = primeDel + helper (primeDel + 1)
            | otherwise                            = helper (primeDel + 1)
