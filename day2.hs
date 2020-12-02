import Text.Regex.Posix

thePattern :: String
thePattern = "([0-9]+)-([0-9]+) (.): (.*)"

parseTxt :: String -> (Int,Int,Char,String)
parseTxt x = let a = x =~ thePattern :: [[String]]
                 in (
                     read . (!!1) . head $ a,
                     read . (!!2) . head $ a,
                     head . (!!3) . head $ a,
                     (!!4) . head $ a
                    )

instCt :: Char -> String -> Int
instCt c = length . (filter (==c))

xor :: Bool -> Bool -> Bool
a `xor` b = (a && (not b)) || (b && (not a))


validPasswd :: (Int,Int,Char,String) -> Bool
validPasswd (a,b,c,s) = let x = instCt c s
                            in x >= a && x <= b

validPasswd2 :: (Int,Int,Char,String) -> Bool
validPasswd2 (a,b,c,s) = let x = s!!(a-1)
                             y = s!!(b-1)
                             in (x==c) `xor` (y==c)

problem1 :: IO ()
problem1 = readFile "day2.txt" >>= (putStrLn . show . length . (filter validPasswd) . (map parseTxt) . lines)

problem2 :: IO ()
problem2 = readFile "day2.txt" >>= (putStrLn . show . length . (filter validPasswd2) . (map parseTxt) . lines)

main :: IO ()
main = problem1 >> problem2