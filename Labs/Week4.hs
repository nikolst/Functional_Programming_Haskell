main :: IO()
main = do

    print (countElements [3, 5, 6, 19])

    print (sumElements [3, 7, 12, 4])

    print (checkFunc 3 [4, 6, 8, 12])
    print (checkFunc 7 [2, 15, 7, 9, 43])

    print (primeList 3 19)

    print (removeElement 4 [3, 6, 4, 2, 1])

    print (removeAllElements 6 [2, 4, 6, 9, 5, 6, 0, 12, 6])

    print (incrementAllBy [8, 2, 4, 7, 1] 3)


{-
Задача 1. 
Да се дефинира функция, която намира броя на елементите на списък.
-}
countElements :: [Integer] -> Integer
countElements xs = if null xs then 0 else 1 + countElements (tail xs)

{-
Задача 2. 
Да се дефинира функция, която намира сумата на елементите в списък.
-}
sumElements :: [Integer] -> Integer
sumElements xs = if null xs then 0 else (head xs) + sumElements (tail xs)

{-
Задача 3.
Да се дефинира функция, която намира дали даден елемент се съдържа в списък.
-}
checkFunc :: Integer -> [Integer] -> Bool
checkFunc x xs 
    | null xs      = False
    | x == head xs = True
    | otherwise    = checkFunc x (tail xs)

{-
Задача 4. 
Да се дефинира функция, която генерира списък с простите числа в интервала [a,b].
-}
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = helper 2
    where
        helper d
            | d == n       = True
            | mod n d == 0 = False
            | otherwise    = helper (d + 1)

primeList :: Integer -> Integer -> [Integer]
primeList a b 
    | a > b     = []
    | isPrime a = a : primeList (a + 1) b
    | otherwise = primeList (a + 1) b
{-
Задача 5. 
Да се дефинира функция, която премахва първия елемент, равен на x, от даден списък.
-}
removeElement :: Integer -> [Integer] -> [Integer]
removeElement _ [] = []
removeElement x (z : xs) = if (x == z) then xs else z : removeElement x xs

{-
Задача 6. 
Да се дефинира функция, която премахва всички елементи на даден списък, които са равни на x.
-}
removeAllElements :: Integer -> [Integer] -> [Integer]
removeAllElements _ [] = []
removeAllElements x (z : xs) = if (x == z) then removeAllElements x xs else z : removeAllElements x xs

{-
Задача 7. 
Да се дефинира функция incrementAllBy :: [Int] -> Int -> [Int], която получава списък и число 
и го добавя към всеки елемент на списъка.
-}
incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy [] _ = []
incrementAllBy xs z = [z + x | x <- xs]
