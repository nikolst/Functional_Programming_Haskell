main :: IO()
main = do
    print (myMin 3 7)
    print (myMin 18 5)

    print (isInside 6 2 9)
    print (isInside 14 4 8)

    print (calcAverage 2 3)

    print (fibRec 5)

    print (fibIter 5)

{-
    Задача 1. 
    Да се дефинира функция myMin, която приема два аргумента и връща по-малкия от тях.
-}

myMin :: Integer -> Integer -> Integer
myMin a b = if (a < b) then a else b

{-
    Задача 2. 
    Да се дефинира функцията isInside x a b, която проверява дали числото x се намира 
    в затворения интервал [a, b].
-}

isInside :: Integer -> Integer -> Integer -> Bool
isInside x a b = if (x >= a && x <= b) then True else False

{-
    Задача 3. 
    Да се дефинира функция calcAverage, която пресмята на средно аритметичното от квадратите на 2 числа.
-}

calcAverage :: Float -> Float -> Float
calcAverage a b = (a ^ 2 + b ^ 2) / 2

{-
    Задача 4. 
    Да се дефинира fibRec, която получава един аргумент n и връща n-тото число на Фибоначи, 
    чрез рекурсивен процес. 
    (Заб.: редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0.)
-}

fibRec :: Integer -> Integer
fibRec n = if (n <= 1) then 1 else fibRec (n - 2) + fibRec (n - 1)

{-
    Задача 5. 
    Да се дефинира fibIter, която получава един аргумент n и връща n-тото число на Фибоначи, 
    чрез итеративен процес.
    (Заб.: редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0.)
-}

fibIter :: Integer -> Integer
fibIter n = helper 1 1 n
    where 
        helper a b n = if (n < 2) then b else helper b (a + b) (n - 1) 
