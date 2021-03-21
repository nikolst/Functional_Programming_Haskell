main :: IO()
main = do

    {-
    Проверки на функцията listTransformation и sumList
    print (listTransform [1, 2, 4, 7, 8])
    print (listTransform [2, 4, 5, 7])
    print (listTransform [8, 4, 5, 7])
    print (listTransform [])
    print (listTransform [1, 2, 4, 7, 8, 6, 2, 1, 45, 78, 9, 12, 5, 3, 67, 89, 12, 33])
    print (sumList [8, 2, 5, 8])
    -}

    print (validate 1714)
    print (validate 2121)
    print (validate 12345)
    print (validate 4736778291034)


-- функцията cardNumber преобразува въведено число в списък от неговите цифри
cardNumber :: Int -> [Int]
cardNumber x
    | x < 10 = [x]
    | otherwise = cardNumber (x `div` 10) ++ [x `mod` 10]

{-
Функцията listTransform преобразува цифрите в списъка в зависимост от техния ред и стойност, но и прави проверка 
дали списъка е празен или дължината му е по-голяма от 16.
Умножаваме всяка втора цифра по 2, като използваме четността на броя цифри в списъка, за да определим дали да започнем 
умножението от първата или от втората цифра в списъка, тъй като по условие е дадено, че умножението е от дясно наляво.
-}
listTransform :: [Int] -> [Int]
listTransform xs
    | xs == []                                   = []
    | length xs > 16                             = []
    | length xs `mod` 2 == 0 && head xs * 2 < 10 = head xs * 2 : listTransform (tail xs)
    | length xs `mod` 2 == 0 && head xs * 2 > 9  = (head xs * 2) `mod` 10 + ((head xs * 2) `div` 10) `mod` 10 : listTransform (tail xs)
    | otherwise                                  = head xs : listTransform (tail xs)

-- функцията sumList сумира елементите на получения след преработка списък от функцията listTransform
sumList :: [Int] -> Int
sumList xs = sum (listTransform xs)

-- функцията validate проверява дали картата е валидна в зависимост от получения остатък при деление на 10
validate :: Int -> Bool
validate n
    | sumList (cardNumber n) `mod` 10 == 0 = True
    | otherwise                            = False
