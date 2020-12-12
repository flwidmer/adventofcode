import Data.List
import Debug.Trace

readInt :: String -> Int
readInt = read

main = do
    print "day 10"
    input <- readFile "input"
    print $ solve1 input
    print $ prepare input

solve1 input =
    let l = lines input
        li = map readInt l -- List Int
        s = sort li -- Sort 
        ss = head s : s -- Add first again
        p = zip s ss -- Make pairs
        diff = map (uncurry (-)) p -- get difference
    in (1 + extract (==1) diff) *(1+ extract (==3) diff) -- complicated math 
    where extract comp list = length $ filter comp list -- find all matching

prepare input =
    let l = lines input
        li = map readInt l -- List Int
        s = sort li -- Sort 
        ss = head s : s -- Add first againoiuztr
        p = zip s ss -- Make pairs
        diff = map (uncurry (-)) p -- get difference
        pos = map fst $ filter (\a -> 3 == snd a) $ zip [0..] diff
    in (ss, pos)

solve2 input =
    let a = prepare input 
        b = snd a 
        c = zip (0:b) b
        d = (0:fst a )
        e = map (\x -> sublist x d) c 
    in product $ map recursion (e)


recursion [_] = 1
recursion (r:rs) =
    let left = (head rs == r + 1 || head rs == r + 3 || head rs == r+ 2)
        middle = 2 <= length rs && rs !! 1 == r + 2
        right = 3 <= length rs && rs !! 2 == r + 3
     -- in (r rsleft right)
     in (if left then recursion rs else 0) + (if right then recursion $ drop 2 rs else 0) + (if middle then recursion $ drop 1 rs else 0)


sublist :: (Int, Int) -> [a] -> [a]
sublist (from, to) = drop from . take to