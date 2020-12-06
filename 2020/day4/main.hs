-- ctrl + shift + shift 
-- cabal update
-- cabal install split regex-posix
import Data.List
import Data.List.Split
import Text.Read 
import Text.Regex.Posix

main = do
        input <- readFile "input"
        putStrLn "asd"
        print length $ filter id $ map passportCheck $ processedPassports input
        print length $ filter id $ map passportCheck2 $ processedPassports input

processedPassports :: String -> [[(String, String)]]
processedPassports input = map passportAllFields $ passports input

-- split file into passports
-- lines is no help here
passports :: String -> [String]
passports = splitOn "\n\n"

-- create a list of tuples representing each field of passport
passportAllFields :: String -> [(String, String)]
passportAllFields passport = 
  let fields = words passport 
      fieldSplits = map (splitOn ":") fields 
      fieldTuples = map (\f -> (head f, f!!1)) fieldSplits
  in fieldTuples

-- check all fields present and valid
-- use fold to check "all true"
passportCheck2 :: [(String, String)] -> Bool
passportCheck2 passportTuples = 
  passportCheck passportTuples && foldl1 and $ map valid passportTuples

passportCheck passportTuples = 
  let keys = map fst passportTuples
      needed = ["byr", "iyr","eyr","hgt","hcl","ecl","pid"]
  in foldl1 and $ map (`elem` keys) needed

-- Validity using pattern matching
-- parse number, check in between
valid ("byr", v) = between 1920 2002 $ readMaybe v
valid ("iyr", v) = between 2010 2020 $ readMaybe v
valid ("eyr", v) = between 2020 2030 $ readMaybe v
-- parse number in either format (the other will be Nothing)
-- between can handle nothing already
valid ("hgt", v) = 
  let formatCm = extractIntFromRegexResult $ getGroupByRegex "^(1[0-9]{2})cm$" v
      formatIn = extractIntFromRegexResult $ getGroupByRegex "^([5-7][0-9])in$" v
  in between 150 193 formatCm || between 59 76 formatIn
-- match regex
valid ("hcl", v) = v =~ "^#[a-f0-9]{6}$"
valid ("ecl", v) = v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
valid ("pid", v) = v =~ "^[0-9]{9}$"
-- always true, format not important
valid ("cid", v) = True
-- any other field is invalid
valid (s, v) = False

-- helper function
between lower upper value
  | Just p <- value = lower <= p && upper >= p
  | otherwise = False

getGroupByRegex :: String -> String -> (String, String, String, [String])
getGroupByRegex regex v = v =~ regex

extractIntFromRegexResult :: (String, String, String, [String]) -> Maybe Int
extractIntFromRegexResult (_,_,_,[]) = Nothing
extractIntFromRegexResult (_,_,_,s) = readMaybe $ head s

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:
-- If cm, the number must be at least 150 and at most 193.
-- If in, the number must be at least 59 and at most 76.
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
-- cid (Country ID) - ignored, missing or not.
