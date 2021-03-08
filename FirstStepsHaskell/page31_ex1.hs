main :: IO()
main = do
    -- a) test
    print (findMaxFunc 12 45 78 23)
    -- b) test
    print (halfPerimeter 4 3 5)
    print (heronsFormula 9 10 11)
    print (heronsFormula 3 2 4)
    print (heronsFormula 14 12 6)

-- a)
-- функция за намиране на по-малкото от две числа
findMinFunc :: Integer -> Integer -> Integer
findMinFunc a b = if (a > b) then b else a

-- функция за намиране на по-голямото от две по-малки числа
findMaxFunc :: Integer -> Integer -> Integer -> Integer -> Integer
findMaxFunc x y z t = if (findMinFunc x y > findMinFunc z t) then findMinFunc x y else findMinFunc z t

-- b) 
--функция за намиране на полупериметъра
halfPerimeter :: Float -> Float -> Float -> Float
halfPerimeter a b c = (a + b + c) / 2;

-- функция за изчисление на Хероновата формула
heronsFormula :: Float -> Float -> Float -> Float
heronsFormula a b c = sqrt(halfPerimeter a b c * (halfPerimeter a b c - a) * (halfPerimeter a b c - b) * (halfPerimeter a b c - c))
