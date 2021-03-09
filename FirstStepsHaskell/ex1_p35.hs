main :: IO()
main = do
    print (mark 6 5 6 6 6)
    print (mark 6 6 6 6 6)
    print (mark 2 2 2 2 2)
    print (mark 2 2 2 2 6)

mark :: Double -> Double -> Double -> Double -> Double -> Double
mark k1 k2 d1 d2 exam = k1 * 5/100 + k2 * 10/100 + d1 * 5/100 + d2 * 10/100 + exam * 70/100 
