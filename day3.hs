import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

coordify :: [String] -> Map (Int,Int) Char
coordify ls = let width = length . head $ ls
                  in Map.fromList [((x,y),(ls!!y)!!(x`mod`width)) | y <- [0..length ls-1], x <- [0..]]

problem1 = readFile "day3.txt" >>= (\txt -> do
    let theLines = lines txt
    let theWidth = (length . head) theLines
    let thePoints = coordify theLines
    let theEncounterPoints = [((3*y)`mod`theWidth,y) | y <- [0..length theLines-1]]
    let theEncounters = [Map.findWithDefault '?' a thePoints | a <- theEncounterPoints] :: String
    putStrLn . show . length . (filter (=='#')) $ theEncounters
    )

getPoints :: (Int,Int) -> [(Int,Int)]
getPoints (a,b) = [(a*t,b*t) | t <- [0..]]

problem2 = readFile "day3.txt" >>= (\txt -> do
    let theLines = lines txt
    let theWidth = (length . head) theLines
    let thePoints = coordify theLines
    let theGradients = [(1,1),(3,1),(5,1),(7,1),(1,2)]
    let theEncounterPoints = [takeWhile (\(x,y) -> y<length theLines) (getPoints (a,b)) | (a,b) <- theGradients]
    let theEncounters = map (map (\(x,y) -> Map.findWithDefault '?' (x`mod`theWidth,y) thePoints)) theEncounterPoints
    putStrLn . show . product . (map (length . (filter (=='#')))) $ theEncounters
    )
main = problem1 >> problem2