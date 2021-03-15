main :: IO()
main = do
    print (tExists 3 4 6)
    print (triangle 3 4 6)
    print (triangle 4 4 4)
    print (triangle 4 4 6)
    print (triangle 2 1 1)

-- check if triangle exists
tExists :: Integer -> Integer -> Integer -> Bool
tExists a b c = a > 0 && b > 0 && c > 0 && (a + b > c) && (a + c > b) && (b + c > a) 

-- check the type of triangle if exists
triangle :: Integer -> Integer -> Integer -> Integer
triangle a b c = if (tExists a b c && a == b && b == c && a == c) then 1 else 
    if (tExists a b c && (a == b || a == c || b == c)) then 2 else
         if (tExists a b c && a /= b && a /= c && b /= c) then 3 else 0
