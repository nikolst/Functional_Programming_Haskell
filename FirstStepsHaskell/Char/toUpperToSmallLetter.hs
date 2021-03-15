main :: IO()
main = do
    print (toNum 'a')
    print (toNum 'w')
    print (toNum 'A')
    print (toChr 98)
    print (toChr 87)
    print (toUpper 'w')
    print (toSmall 'G')


toNum :: Char -> Int
toNum = fromEnum

toChr :: Int -> Char
toChr = toEnum

toUpper :: Char -> Char
toUpper ch = toChr(toNum ch + toNum 'A' - toNum 'a')

toSmall :: Char -> Char
toSmall ch = toChr(toNum ch + toNum 'a' - toNum 'A')
