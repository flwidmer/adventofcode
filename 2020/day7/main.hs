{-# LANGUAGE TupleSections #-}
import Data.List.Split ( splitOn )
import Data.List ( nub )

main :: IO ()
main = do
    input <- readFile "input"
    print "day7"
    print $ solve1 input
    print $ solve2 input


solve1 :: String -> Int
solve1 input =
    let assocList = createInvertedMap $ map parseRule $ lines input
        recursion = recurse1 assocList "shinygold"
    in length $ nub recursion

recurse1 :: [(String, String)] -> String -> [String]
recurse1 assocList search  =
    let current = lookupAll assocList search
        next = concatMap (recurse1 assocList) current
    in current ++ next

solve2 :: String -> Int
solve2 input =
    let assocList = map parseRule $ lines input
        recursion = recurse2 assocList "shinygold"
    in recursion

recurse2 :: [(String, [(String, Int)])] -> String -> Int
recurse2 assocList search  =
    let current = concat $ lookupAll assocList search
        next = sum $ map recurseValue current
    in sum (map snd current) + next
    where recurseValue (bag, multiplier) = multiplier * recurse2 assocList bag

-- parse one line into an association list
parseRule :: String -> (String, [(String, Int)])
parseRule a =
    let keyValue = splitOn " contain " a
        key = concat $ take 2 $ words $ head keyValue
        value =map parseContains $ splitOn "," $ keyValue !! 1
    in (key , value)

-- "contain 2 shiny gold bags." -> ("shinygold", 2)
parseContains :: String -> (String, Int)
parseContains "no other bags." = ("none", 0)
parseContains a =
    let removePeriod = filter (/= '.') a
        noBags = filter (\x -> x /="bags" && x /= "bag") $ words removePeriod
    in (concat $ tail noBags, read $ head noBags)

-- unwrap the association list
createInvertedMap :: [(String, [(String, b)])] -> [(String, String)]
createInvertedMap = concatMap invert
    where invert (outer, inner) = map ((, outer) . fst) inner

-- a lookup that returns more than one result
lookupAll :: [(String, b)] -> String -> [b]
lookupAll assocList key  = map snd $ filter (\(k,_) -> k == key) assocList

