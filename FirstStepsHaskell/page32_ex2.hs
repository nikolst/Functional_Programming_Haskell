main :: IO()
main = do
    print (p2 1 2 3)
    print (tExistation 3 4 6)
    print (tExistation' 3 4 6)

{-
    p2 е функция за полупериметъра на триъгълник със страни a, b, c
    surfaceT е функция за намиране на лице на триъгълник със страни a, b, c
    tExistation и tExistation' са функции за извеждане на периметъра и лицето на триъгълник в списък 
-}


-- без локална дефиниция
p2 :: Float -> Float -> Float -> Float
p2 a b c = (a + b + c) / 2

surfaceT :: Float -> Float -> Float -> Float
surfaceT a b c = sqrt(p2 a b c * (p2 a b c - a) * (p2 a b c - b) * (p2 a b c - c))

tExistation :: Float -> Float -> Float -> [Float]
tExistation a b c
    | (a /= 0) && (b /= 0) && (c/=0) && (a + b > c) && (a + c > b) && (b + c > a) = [(a + b + c), surfaceT a b c]
    | otherwise = []


-- с локална дефиниция
tExistation' :: Float -> Float -> Float -> [Float]
tExistation' a b c
    | (a /= 0) && (b /= 0) && (c/=0) && (a + b > c) && (a + c > b) && (b + c > a) 
        = [(p * 2), sqrt(p * (p - a) * (p - b) * (p - c))]
    | otherwise = []
    where p = (a + b + c) / 2
