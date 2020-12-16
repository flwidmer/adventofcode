import Data.List
import qualified Data.IntMap.Strict as M

main = do
    print "day15"
    print solve1
    print solve2

test1 = foldl' speak (M.fromList [(0,[4,1]),(6,[3]),(3,[2])], 0) [5..10]

input = M.fromList [(19,[7]),(14,[6]),(12,[5]),(18,[4]),(0,[3]),(1,[2]),(16,[1])]

solve1 = snd $ foldl' speak (input, 19) [8..2020]

solve2 = snd $ foldl' speak (input, 19) [8..30000000]



speak :: (M.IntMap [Int], Int) -> Int -> (M.IntMap [Int], Int)
speak (state, lastNum) pos =
    let say = lastSpoken $ M.lookup lastNum state
        inMap = M.member say state
        newState = if inMap then M.updateWithKey (updateMap pos) say state else M.insert say [pos] state
    in (newState, say)
    where
        lastSpoken Nothing = 0
        lastSpoken (Just a)
            | length a >= 2 = head a - a !! 1
            | otherwise = 0

updateMap :: Int -> Int -> [Int] -> Maybe [Int]
updateMap val _ oldVal = Just (val:oldVal)

whatToSay :: Int -> Maybe (Int, Int) -> (Int, Int)
whatToSay pos Nothing = (0, pos)
whatToSay pos (Just (_, aNum)) = (pos - 1 - aNum, pos)
