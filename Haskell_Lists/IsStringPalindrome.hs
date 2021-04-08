main :: IO()
main = do
    print (isStringPalin "abba")
    print (isStringPalin "abcd")
    print (isStringPalin "abna")
    print (isStringPalin "abnba")
   
isStringPalin :: String -> Bool
isStringPalin st = 
    if ((head st : tail st) == (head (reverse st) : tail (reverse st))) then True else False


