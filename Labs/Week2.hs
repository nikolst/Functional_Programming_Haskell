main :: IO()
main = do
    print (myGCD 25 5)
    print (myGCD 13 39)
    print (myGCD 18 42)

    print (countDigits 123)
    print (countDigits 34)

    print (sumDigitsRec 145)
    print (sumDigitsRec 7)
    print (sumDigitsRec 78354)

    print (sumDigitsIter 5632)

    print (reverseNumber 567)

    print (isPalindorme 11211)
    
    print (countPalindromes 12 900)
    print (countPalindromes 12 7)

{-
    Задача 1. 
    Да се дефинира функция myGCD a b, която връща най-големия общ делител на числата a и b.
-}

myGCD :: Integer -> Integer -> Integer
myGCD a b = if (a == b) then a else     
                if (a < b) then myGCD a (b - a) else myGCD (a - b) b

{-
    Задача 2. 
    Да се дефинира функция countDigits, която генерира линейно рекурсивен процес 
    и намира броя на цифрите на дадено естествено число.
-}

countDigits :: Integer -> Integer
countDigits x = if (x /= 0) then 1 + countDigits (x `div` 10) else 0

{-
    Задача 3. 
    Да се дефинира функция sumDigitsRec, която генерира линейно рекурсивен процес и 
    намира сумата от цифрите на дадено естествено число.
-}

sumDigitsRec :: Integer -> Integer
sumDigitsRec n = if (n == 0) then 0 else n `mod` 10 + sumDigitsRec (n `div` 10)

{-
    Задача 4. 
    Да се дефинира функция sumDigitsIter, която генерира линейно итеративен процес и 
    намира сумата от цифрите на дадено естествено число.
-}

sumDigitsIter :: Integer -> Integer
sumDigitsIter n = iter n 0
    where iter n result = if (n == 0) then result else iter (n `div` 10) (result + (n `mod` 10))

{-
    Задача 5. 
    Да се дефинира функция reverseNumber, която генерира линейно итеративен процес и 
    по дадено естествено число n намира числото, записано със същите цифри, но в обратен ред.
-}

reverseNumber :: Integer -> Integer
reverseNumber n = helper n 0
    where helper n  result = if (n == 0) then result else helper (n `div` 10) (result * 10 + n `mod` 10)

{-
    Задача ++.
    Дефинирайте двуаргументна функция countPalindromes a b, която намира броя на
    числата палиндроми в затворения интервал [a, b].
-}

isPalindorme :: Integer -> Bool
isPalindorme x = x == reverseNumber x

countPalindromes :: Integer -> Integer -> Integer
countPalindromes a b 
    | a > b          = 0
    | isPalindorme a = 1 + countPalindromes (a + 1) b
    | otherwise      = countPalindromes (a + 1) b
