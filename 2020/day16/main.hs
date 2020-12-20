import Data.List.Split ( splitOn )
import Data.List ((\\), sortBy)
import Data.Function (on)


main = do
    input <- readFile "input"
    print "day16"
    print $ solve1 input
    print $ solve2 input

solve2 inp =
    let (rules, mine, nearby) = parseInput inp
        onlyValid = filterInvalid rules nearby -- only valid tickets
        ticketLength = length mine
        slices = map (slice onlyValid) [0..ticketLength -1] -- slice vertically 
        orderedList = findOrderedListOfRules rules slices -- ordered list of slices and matching rules
        tempSlices = map fst orderedList -- extract slices from that list 
        tempRules = map snd orderedList -- extract rules from that list
        uniqueRules = map head $ zipWith (\\)  tempRules ([]:tempRules) -- find the uniquely matching rules on that list [19] and [19.7] will return 7.
        slice_rule = zip tempSlices uniqueRules -- match again with the slices
        interesting_slice_rule = filter (\x -> snd x `elem` [0..5]) slice_rule -- only the first six are interesting
        values = map ((mine !!) . fst) interesting_slice_rule -- extract the values of those
    in product values -- profit!
    where slice ll s = map (!!s) ll

-- validate a rule for an entire slice. True iff all are true
validateEntireSlice ::  [Int] -> (String, [(Int, Int)]) -> Bool
validateEntireSlice slice rule = all (`validateRule` rule) slice

-- find all rules that match the given slice
findMatchingRules rules slice  =
    let matching = map (validateEntireSlice slice) rules
        numbered = zip [0..] matching
    in map fst $ filter snd numbered

-- order the list of matching rules by size. shortest first for all slices
findOrderedListOfRules rules slices =
    let matchingRules = map (findMatchingRules rules) slices
        zipped = zip [0..] matchingRules
    in sortBy (compare `on` length . snd) zipped

solve1 inp =
    let (rules, _, nearby) = parseInput inp
        v = map (validate rules) $ concat nearby
        con = concat nearby
        z = zip con v
        allFalse = filter (\(_, v) -> True `notElem` v) z
    in sum $ map fst allFalse

filterInvalid :: [(String, [(Int, Int)])] -> [[Int]] -> [[Int]]
filterInvalid rules nearby =
    let v = map (validateAny rules) nearby
        z = zip nearby v
    in map fst $ filter snd z

validateAny :: [(String, [(Int, Int)])] -> [Int] -> Bool
validateAny rules fields =
    let a = map (validate rules) fields
        f = filter or a
    in length f == length fields

validate :: [(String, [(Int, Int)])] -> Int -> [Bool]
validate rules ticketFields = map (validateRule ticketFields) rules


validateRule :: Int -> (String, [(Int, Int)]) -> Bool
validateRule val (_, rules) = any (valid val) rules
    where valid a (l,u)  = a >= l && a <= u


parseInput :: String -> ([(String, [(Int, Int)])], [Int], [[Int]])
parseInput input =
    let [rules, mine, nearby] = splitOn "\n\n" input
    in (parseRules rules, head $ parseTick mine,parseTick nearby)

parseRules :: String -> [(String, [(Int, Int)])]
parseRules inp =
    let l = lines inp
    in map parseField l
    where
        parseField s =
            let [fieldName, r] = splitOn ": " s
                rules = splitOn " or " r
                pRules = map parseRules rules
            in (fieldName, pRules)
        parseRules rl =
            let [lower,upper] = splitOn "-" rl
            in (read lower, read upper)

parseTick :: String -> [[Int]]
parseTick inp =
    let l = lines inp
        r = tail l
        is = map (splitOn ",") r
        ns = map (map read) is
    in ns
