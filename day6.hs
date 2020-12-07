import Data.List
import Data.List.Split

elemEach :: (Eq a) => a -> [[a]] -> Bool
elemEach x = foldl (\acc xs -> acc && (x `elem` xs)) True

problem1 = readFile "day6.txt" >>= (
      putStrLn
    . show
    . sum
    . (map $ length . nub . concat . (splitOn "\n"))
    . splitOn "\n\n"
    )

problem2 = readFile "day6.txt" >>= (
      putStrLn
    . show
    . sum
    . (map $ length . nub . (\line -> filter (`elemEach`line) ['a'..'z']) . (filter (/="")) . (splitOn "\n"))
    . splitOn "\n\n"
    )

main = problem1 >> problem2