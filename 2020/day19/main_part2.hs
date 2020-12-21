import Data.List.Split (splitOn)
import Text.Regex.Posix
import Debug.Trace

import Data.List ( intercalate )

main = do
    print "day19"
    input <- readFile "input"
    input2 <- readFile "input2"
    print $ length $ filter id $ solve input
    print $ length $ filter id $ solve input2

solve input =
    let (r:t:_) = separate input
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
-- fuck it, we're going cheap!
buildListRegex allRules lst 11 =
    let resolved = map (`lookup` allRules) lst
        withoutThisOne = filter not11 resolved
        unwrapped = map (unwrap allRules 11) withoutThisOne
        parts = map (cheat (head unwrapped) (unwrapped !! 1)) [1..20]
    in trace ("parts: " ++ show  parts) $ "(" ++ intercalate "|" parts ++ ")"
    where
        not11 (Just (Rule _ _ _ n)) = n /= 11
        not11 _ = False


buildListRegex allRules lst num =
    let resolved = map (`lookup` allRules) lst
        parts = map (unwrap allRules num) resolved
    in "(" ++ concat parts ++ ")"

cheat :: String -> String -> Int -> String
cheat left right n = "(" ++ concat (replicate n left) ++ concat (replicate n right) ++ ")"

unwrap allRules num (Just rule)
    | num == extr rule = "+" -- if the rule equals the rule that we are treating, it's the case of "8" in the example.  use a +
    | otherwise = buildRegex allRules rule
    where extr (Rule _ _ _ n) = n
unwrap _ _ Nothing = ""

separate :: String -> [String]
separate = splitOn "\n\n"

parseRules :: String -> [(Int, Rule)]
parseRules i =
    let l = lines i
    in map parseRuleLine l

parseRuleLine :: String -> (Int, Rule)
parseRuleLine i =
    let (num:str:_) = splitOn ":" i
    in (read num, parseRule (read num) str)

data Rule = Rule String ([Int], [Int]) [Int] Int deriving Show

emptySubrule = ([],[])

parseRule :: Int -> String -> Rule
parseRule n i
    | '"' `elem` i   = Rule (filter (/= '\"') $ head $ words i) emptySubrule [] n
    | '|' `elem` i    = Rule "" (parseSubRules i) [] n
    | otherwise     = Rule "" emptySubrule (map read $ words i) n

parseSubRules :: String -> ([Int], [Int])
parseSubRules i =
    let (frst:scnd:_) = splitOn "|" i
    in (extract frst,extract scnd)
    where extract a = map read $ words a