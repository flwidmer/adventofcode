import Data.List

main = do
    print "day15"
    print solve1
    print solve2

test1 = take 10 $ foldl' speak [(0,4),(6,3),(3,2),(0,1)] [5..10]

solve1 = take 10 $ foldl' speak [(19,7),(14,6),(12,5),(18,4),(0,3),(1,2),(16,1)] [8..2020]

solve2 = take 1 $ foldl' speak [(19,7),(14,6),(12,5),(18,4),(0,3),(1,2),(16,1)] [8..30000000]

test2 x = take 1 $ foldl' speak [(19,7),(14,6),(12,5),(18,4),(0,3),(1,2),(16,1)] [8..x]


speak :: [(Int, Int)] -> Int -> [(Int, Int)] 
speak state pos =
    let lastNum = fst $ head state
        wasLastSpoken = find (\(a,_) -> a == lastNum) $ tail state 
        newState = whatToSay pos wasLastSpoken
        
    in (newState:state)
    
whatToSay :: Int -> Maybe (Int, Int) -> (Int, Int)
whatToSay pos Nothing = (0, pos)
whatToSay pos (Just (_, aNum)) = (pos - 1 - aNum, pos)

{- 
Turn 5: Next, again consider the last number spoken, 0. 
Since it had been spoken before, 
the next number to speak is the difference between the turn number when it was last spoken 
(the previous turn, 4) and the turn number of the time it was most recently spoken before 
then (turn 1). Thus, the 5th number spoken is 4 - 1, 3. -}