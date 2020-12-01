prodPairs :: Int -> [Int] -> [(Int,Int)]
prodPairs n xs = [(x,y) | x <- xs, y <- xs, x+y == n]

prodTrips :: Int -> [Int] -> [(Int,Int,Int)]
prodTrips n xs = [(x,y,z) | x <- xs, y <- xs, z <- xs, x+y+z == n]

problem1 = do
    txt <- readFile "day1.txt"
    let theLines = lines txt
    let theNums = map read theLines :: [Int]
    (putStrLn . show . (\(x,y) -> x*y) . head . (prodPairs 2020)) theNums

problem2 = do
    txt <- readFile "day1.txt"
    let theLines = lines txt
    let theNums = map read theLines :: [Int]
    (putStrLn . show . (\(x,y,z) -> x*y*z) . head . (prodTrips 2020)) theNums

main = do
    problem1
    problem2