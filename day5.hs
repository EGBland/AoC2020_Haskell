import Data.List (sort)

partition :: Bool -> (Int,Int) -> (Int,Int)
partition isLeft (x,y)
    | isLeft = (x,y-len)
    | otherwise = (x+len,y)
    where len = 1+(y-x)`div`2

position :: (String,String) -> (Int,Int)
position (a,b) = (fst $ foldl (\x c -> partition (c=='F') x) (0,127) a, fst $ foldl (\x c -> partition (c=='L') x) (0,7) b) 

getId :: (Int,Int) -> Int
getId (x,y) = 8*x+y

diffs :: [Int] -> [(Int,Int,Int)]
diffs [] = []
diffs [x] = []
diffs [x,y] = []
diffs (x:y:z:xs) = (x-y,y,z-y):diffs ([y,z]++xs)

problem1 = readFile "day5.txt" >>= (
      putStrLn
    . show
    . maximum
    . (map $ getId . position . (\l -> (take 7 l, drop 7 l)))
    . lines
    )

problem2 = readFile "day5.txt" >>= (
      putStrLn
    . show
    . (+1)
    . head
    . (map (\(_,y,_) -> y))
    . (filter (\(_,_,z) -> z == 2))
    . diffs
    . sort
    . (map $ getId . position . (\l -> (take 7 l, drop 7 l)))
    . lines
    )

main = problem1 >> problem2