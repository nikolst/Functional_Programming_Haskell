main :: IO()
main = do 
    print (func 3)
    print (func 1)
    print (func 1.5)
    print (func (-4))

func :: Double -> Double
func x 
    | x > 2            = x ^ 2 - x + 4
    | x >= 1 && x <= 2 = x + 7
    | x < 1            = 0
