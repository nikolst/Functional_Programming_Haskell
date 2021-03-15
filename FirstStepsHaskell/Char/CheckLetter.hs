main :: IO()
main = do
    print (isUpper 'G')
    print (isUpper 'y')
    print (isLower 'q')
    print (isLower 'T')
    print (isLatin 'o')
    print (isLatin '@')


isUpper :: Char -> Bool
isUpper ch = (ch >= 'A') && (ch <= 'Z')

isLower :: Char -> Bool
isLower ch = (ch >= 'a') && (ch <= 'z')

isLatin :: Char -> Bool 
isLatin ch = (isLower ch) || (isUpper ch)
