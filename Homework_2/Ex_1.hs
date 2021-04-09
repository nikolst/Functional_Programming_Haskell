main :: IO()
main = do
    {- Test the helper functions:
    print (findP "hhePhsa")    
    print (findP "Phsjsja") 
    print (takeLeft "hhePhsa")
    print (takeRight "hhePhsa")
    print (countRight ")()) (())(P)()( ")
    print (countLeft ")()) (())(P)()( ")
    -}
    print (countRats ")1)1)1)1 P")
    print (countRats "P 1( 1( )1 1(")
    print (countRats "  P 1( 1( )1 1(")
    print (countRats ")1)1)1)1P)1)11(")
    print (countRats "1()1)1)11(1()1)1P)11()1)1)11(1(1(1(")


-- Задача 1:

-- start form 0, because we will not include P in our new lists
findP :: String -> Int
findP [] = 0
findP str = helper str 0
    where 
        helper str count
            | head str == 'P' = count
            | otherwise       = helper (tail str) (count + 1)

-- take the elements before P
takeLeft :: String -> String
takeLeft [] = []
takeLeft xs = take (findP xs) xs

-- take the elements after P
takeRight :: String -> String
takeRight [] = []
takeRight xs = drop ((findP xs) + 1) xs

countRight :: String -> Int
countRight [] = 0
countRight xs = helper (takeRight xs) 0
    where 
        helper xs count
            | xs == []       = count
            | head xs == ')' = helper (tail xs) (count + 1) 
            | otherwise      = helper (tail xs) count 


countLeft :: String -> Int 
countLeft [] = 0
countLeft xs = helper (takeLeft xs) 0
    where 
        helper xs count 
            | xs == []       = count
            | head xs == '(' = helper (tail xs) (count + 1)
            | otherwise      = helper (tail xs) count 


countRats :: String -> Int
countRats [] = 0
countRats xs = countRight xs + countLeft xs
