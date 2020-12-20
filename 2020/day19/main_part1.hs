import Data.List.Split (splitOn)
import Debug.Trace
import Text.Regex.Posix

main = do
  print "day19"
  input <- readFile "input"
  print $ length $ filter id $ solve input

solve input =
  let (r : t : _) = separate input
      allRules = parseRules r
      ruleZero = snd $ head $ filter (\x -> fst x == 0) allRules --cheapo!
      regX = buildRegex allRules ruleZero
   in trace regX $ map (mtch regX) (lines t)

mtch :: String -> String -> Bool
mtch reg str = str =~ ("^" ++ reg ++ "$")

buildRegex :: [(Int, Rule)] -> Rule -> String
buildRegex _ (Rule letter ([], []) [] _) = letter
buildRegex allRules (Rule "" ([], []) lst num) = buildListRegex allRules lst num
buildRegex allRules (Rule "" (lstA, lstB) _ num) = buildOrRegex allRules lstA lstB num

buildOrRegex allRules lstA lstB num =
  let a = buildListRegex allRules lstA num
      b = buildListRegex allRules lstB num
   in "(" ++ a ++ "|" ++ b ++ ")"

buildListRegex :: [(Int, Rule)] -> [Int] -> Int -> String
buildListRegex allRules lst num =
  let resolved = map (`lookup` allRules) lst
      parts = map (unwrap allRules num) resolved
   in "(" ++ concat parts ++ ")"

unwrap allRules num (Just rule)
  | num == extr rule = "+"
  | otherwise = buildRegex allRules rule
  where
    extr (Rule _ _ _ n) = n
unwrap _ _ Nothing = ""

separate :: String -> [String]
separate = splitOn "\n\n"

parseRules :: String -> [(Int, Rule)]
parseRules i =
  let l = lines i
   in map parseRuleLine l

parseRuleLine :: String -> (Int, Rule)
parseRuleLine i =
  let (num : str : _) = splitOn ":" i
   in (read num, parseRule (read num) str)

data Rule = Rule String ([Int], [Int]) [Int] Int deriving (Show)

emptySubrule = ([], [])

parseRule :: Int -> String -> Rule
parseRule n i
  | '"' `elem` i = Rule (filter (/= '\"') $ head $ words i) emptySubrule [] n
  | '|' `elem` i = Rule "" (parseSubRules i) [] n
  | otherwise = Rule "" emptySubrule (map read $ words i) n

parseSubRules :: String -> ([Int], [Int])
parseSubRules i =
  let (frst : scnd : _) = splitOn "|" i
   in (extract frst, extract scnd)
  where
    extract a = map read $ words a