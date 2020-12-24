
import Debug.Trace (trace)

main = do
    print "day 23"
    print $ solve1 puzzle
    print $ solve2 puzzle2

puzzle :: [Int]
puzzle = [2,4,7,8,1,9,3,5,6]

test :: [Int]
test = [3,8,9,1,2,5,4,6,7]

puzzle2 :: [Int]
puzzle2 = puzzle ++ [10..1000000]


solve1 input = 
    let (_, endState) = game1 1 (0, input)
        from1 = tail $ dropWhile  (1/=) $ cycle endState   
    in concatMap show $ take 8 from1
    
solve2 input = 
    let (_, endState) = game2 1 (0, input)
        from1 = tail $ dropWhile  (1/=) $ cycle endState   
    in take 2 from1

game1 100 s = move s 
game1 n s = 
    let nextS = move s 
        nextN = n + 1
    in game1 nextN nextS

game2 10000000 s = move s 
game2 n s = 
    let nextS = move s 
        nextN = n + 1
    in game2 nextN nextS

-- debug _ b = b
debug a b = trace a b


move :: (Int, [Int]) -> (Int, [Int])
move (indexCurrent, state) =
    let l = length state
        current = state !! indexCurrent
        toPickup = map ((state !!) . offset indexCurrent l) [1,2,3]
        stateWithoutPickup = filter (`notElem` toPickup) state
        destination = getDestination current stateWithoutPickup
        destinationIndex = indexof destination stateWithoutPickup
        (before, after) = splitAt (destinationIndex +1) stateWithoutPickup
        nextState = (before ++ toPickup ++ after)
        indexCurrentNew = indexof current nextState
        indexNextCup = offset indexCurrentNew l 1
    in (indexNextCup, nextState)

offset index l offs = (index + offs) `mod` l

indexof item list = fst $ head $ filter (\x -> snd x == item) $ zip [0..] list

getDestination current state
    | (current - 1) `elem` state = current - 1
    | current == minimum state = maximum state
    | otherwise = getDestination (current - 1) state


{- The crab picks up the three cups that are immediately clockwise of the current cup. 
They are removed from the circle; cup spacing is adjusted as necessary to maintain the circle.

The crab selects a destination cup: the cup with a label equal to the current cup's label minus one. 
If this would select one of the cups that was just picked up, the crab will keep subtracting one until 
it finds a cup that wasn't just picked up. If at any point in this process the value goes below the 
lowest value on any cup's label, it wraps around to the highest value on any cup's label instead.

The crab places the cups it just picked up so that they are immediately clockwise of the destination 
cup. They keep the same order as when they were picked up.

The crab selects a new current cup: the cup which is immediately clockwise of the current cup. -}