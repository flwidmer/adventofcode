import Data.List.Split
import Debug.Trace

main = do
    input <- readFile "input"
    print "day 22"
    print $ solve1 input
    print $ solve2 input


debug _ b = b
-- debug a b = trace a b

solve1 input =
    let (s1,s2) = parseStacks input
        winner = play1 s1 s2
        l = length winner
    in sum $ zipWith (*) [l, l-1..1] winner

solve2 input = 
    let (s1,s2) = parseStacks input
        (a,b) = play2 s1 s2 []
    -- in (a,b)
    in max (score a) (score b)
    where 
        score x = 
            let l = length x 
            in sum $ zipWith (*) [l, l-1..1] x

play1 player1 [] = player1
play1 [] player2 = player2
play1 player1 player2 =
    let p1 = head player1
        p2 = head player2
    in if p1 > p2
        then play1 (tail player1 ++ [p1,p2]) (tail player2)
        else play1 (tail player1) (tail player2 ++ [p2,p1])

play2 :: [Int] -> [Int] -> [([Int], [Int])] -> ([Int],[Int])
play2 [] player2 _ = ([], player2)
play2 player1 [] _ = (player1, [])
play2 player1 player2 decks =
    let p1 = head player1
        p2 = head player2
        l1 = length $ tail player1
        l2 = length $ tail player2
    in if (player1, player2) `elem` decks then debug "same" (player1, []) 
    else debug ("p2: "++ show player1 ++ show player2 ++ " p1: " ++ show p1 ++ " l1: "++ show l1 ++ " p2: " ++ show p2 ++ " l2: " ++ show l2 )(
        if p1 <= l1 && p2 <= l2 
            then recursiveCombat p1 p2 (tail player1) (tail player2) decks 
            else regularCombat player1 player2 decks)

regularCombat :: [Int] -> [Int] -> [([Int], [Int])] -> ([Int],[Int])
regularCombat player1 [] _ = (player1, [])
regularCombat [] player2 _ = ([], player2)
regularCombat player1 player2 decks =
    let p1 = debug ("reg: "++ show player1 ++ show player2) head player1
        p2 = head player2
        newDecks = ((player1, player2) : decks)
    in if p1 > p2
        then play2 (tail player1 ++ [p1,p2]) (tail player2) newDecks
        else play2 (tail player1) (tail player2 ++ [p2,p1]) newDecks

recursiveCombat :: Int -> Int -> [Int] -> [Int] -> [([Int], [Int])] -> ([Int], [Int])
recursiveCombat p1 p2 player1 player2 decks =
    let (w1,_) = debug ("rec: "++ show player1 ++ show player2) play2 (take p1 player1) (take p2 player2) []
        win1 = not (null w1)
        newDecks = ((player1, player2) : decks)
    in if win1
        then play2 (player1 ++ [p1,p2]) player2 newDecks
        else play2 player1 (player2 ++ [p2,p1]) newDecks

parseStacks :: String -> ([Int], [Int])
parseStacks input =
    let (stack1:stack2:_) = splitOn "\n\n" input
        s1 = readCards stack1
        s2 = readCards stack2
    in (s1, s2)
    where readCards x = cards $ tail $ lines x

cards :: [String] -> [Int]
cards = map read
