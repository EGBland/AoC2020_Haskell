import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Text.Regex.PCRE

type Relation a b = [(a,b)]

ruleRegex = "^([a-z ]+) bags contain ((?:no other bags|[0-9]+ [a-z ]+ bags?,? ?)+)\\.$":: String
bagRegex = "[0-9]+ ([a-z ]*) bags?" :: String

procRule :: String -> Relation String String
procRule s = let (_,_,_,matches) = s =~ ruleRegex :: (String,String,String,[String])
                 bagLeft = matches!!0
                 bagsRight = matches!!1
                 bagsRightFields = splitOn ", " bagsRight
                 bagsRightMatcherLst = [bag =~ bagRegex :: (String,String,String,[String]) | bag <- bagsRightFields]
                 bagsRightLst = map (\(_,_,_,w) -> concat w) bagsRightMatcherLst
                 in (map (\x -> (bagLeft,x))) . (filter (/="")) $ bagsRightLst

adjacents :: (Eq a) => [a] -> Relation a b -> Relation a b
adjacents xs rs = concat [adjacents' x rs | x <- xs]

adjacents' :: (Eq a) => a -> Relation a b -> Relation a b
adjacents' x = filter $ (==x) . fst

problem1 = readFile "day7.txt" >>= (\txt -> do
    let theLines = lines txt
    let theRules = (map swap) . concat . (map procRule) $ theLines
    let theIterations = iterate ((map snd) . ((flip adjacents) theRules)) ["shiny gold"]
    putStrLn . show . length . nub . concat $ takeWhile (/=[]) (tail theIterations)
    )

problem2 = return ()

main = problem1 >> problem2