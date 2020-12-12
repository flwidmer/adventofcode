import Data.List ( tails )

main :: IO ()
main = do
    input <- readFile "input"
    print $ solve1 25 input
    print $ map (`solve2` input) (solve1 25 input)


solve1 :: Int -> String -> [Maybe Int]
solve1 num input =
    let ll = readIn input
        tester = test num ll
    in filter (/= Nothing) $ map tester [num..length ll-1]

solve2 :: Maybe Int -> String -> [Int]
solve2 Nothing _ = []
solve2 (Just num) input =
    let ll = readIn input
        ta = tails ll
        cand = filter (\x -> length x /= 1) $ map head $ filter (/= []) $ map findCand ta
    in map (\x -> minimum x + maximum x) cand
    where 
        findCand l = filter (\x -> sum x == num) $ tails $ reverse l

readIn :: String -> [Int]
readIn a = map read $ lines a

test :: Int -> [Int] -> Int -> Maybe Int
test n l idx =
    let (left, right) = splitAt idx l
        scope = take n $ reverse left
        toTest = head right
    in check scope toTest

check :: [Int] -> Int -> Maybe Int
check scope toTest
    | test scope toTest = Nothing
    | otherwise = Just toTest
    where
        test a b = b `elem` possibilities a
        possibilities x = [z+y|z <-x, y <- x ]
