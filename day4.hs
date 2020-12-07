import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Text.Regex.Posix

type Passport = Map String String

isValidPassport :: Passport -> Bool
isValidPassport pass = Map.member "ecl" pass
                    && Map.member "pid" pass
                    && Map.member "eyr" pass
                    && Map.member "hcl" pass
                    && Map.member "byr" pass
                    && Map.member "iyr" pass
                    
                    && Map.member "hgt" pass

isValidEcl :: String -> Bool
isValidEcl = (=~ "^(amb|blu|brn|gry|grn|hzl|oth)$")

isValidByr :: String -> Bool
isValidByr s = let x :: Int
                   x = read s
                   in 1920 <= x && x <= 2002

isValidIyr :: String -> Bool
isValidIyr s = let x :: Int
                   x = read s
                   in 2010 <= x && x <= 2020

isValidEyr :: String -> Bool
isValidEyr s = let x :: Int
                   x = read s
                   in 2020 <= x && x <= 2030

isValidHgt :: String -> Bool
isValidHgt s
    | s =~ "^[0-9]+cm$" = let x :: Int
                              x = read (take (length s-2) s)
                              in 150 <= x && x <= 193
    | s =~ "^[0-9]+in$" = let x :: Int
                              x = read (take (length s-2) s)
                              in 59 <= x && x <= 76
    | otherwise = False

isValidHcl :: String -> Bool
isValidHcl = (=~ "^#[0-9a-f]{6}$")

isValidPid :: String -> Bool
isValidPid = (=~ "^[0-9]{9}$")

isValidPassport2 :: Passport -> Bool
isValidPassport2 pass = isValidEcl (Map.findWithDefault "" "ecl" pass)
                     && isValidByr (Map.findWithDefault "0" "byr" pass)
                     && isValidIyr (Map.findWithDefault "0" "iyr" pass)
                     && isValidEyr (Map.findWithDefault "0" "eyr" pass)
                     && isValidHgt (Map.findWithDefault "" "hgt" pass)
                     && isValidHcl (Map.findWithDefault "" "hcl" pass)
                     && isValidPid (Map.findWithDefault "" "pid" pass)
   
problem1 = readFile "day4.txt" >>= (\txt -> do
    let thePassports = splitOn "\n\n" txt
    let theFieldsSanitised = map (map (\x -> if x == '\n' then ' ' else x)) thePassports
    let theFields = map ((filter (/="")) . (splitOn " ")) theFieldsSanitised
    let theSplits = map (map (splitOn ":")) theFields
    let thePairs = map (map (\xs -> (xs!!0,xs!!1))) theSplits
    let thePassports = map (Map.fromList) thePairs
    putStrLn . show . length . (filter isValidPassport) $ thePassports
    )

problem2 = readFile "day4.txt" >>= (\txt -> do
    let thePassports = splitOn "\n\n" txt
    let theFieldsSanitised = map (map (\x -> if x == '\n' then ' ' else x)) thePassports
    let theFields = map ((filter (/="")) . (splitOn " ")) theFieldsSanitised
    let theSplits = map (map (splitOn ":")) theFields
    let thePairs = map (map (\xs -> (xs!!0,xs!!1))) theSplits
    let thePassports = map (Map.fromList) thePairs
    putStrLn . show . length . (filter isValidPassport2) $ thePassports
    )

main = problem1 >> problem2