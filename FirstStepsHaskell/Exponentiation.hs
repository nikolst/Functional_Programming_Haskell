main :: IO()
main = do
    print (expo 3 4)
    print (expo' 2 3)
    print (expo' 5 3)

{-
  x is the base
  n is the power
-}

-- 1st way
expo :: Int -> Int -> Int
expo x n = if (x == 1 || n == 0) then 1 else x ^ n


-- 2nd way
expo' :: Int -> Int -> Int
expo' x n = if (x == 1 || n == 0) then 1 else x * expo' x (n - 1)
