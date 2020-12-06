main = do
        input <- readFile "input"
        -- solve $ inputList input
        print $ trees $ solve 1 3 $ lines input
        print $ solve2 input

solve2 inp =
   let  input = lines inp
        one = trees $ solve 1 1 input
        two = trees $ solve 1 3 input
        three = trees $ solve 1 5 input
        four = trees $ solve 1 7 input
        five = trees $ solve 2 1 input
    in one * two * three * four * five

solve stepDown stepLeft l =
   let len = length $ head l
       filtered = filterList stepDown l
       coord = zipWith (\ a b -> (calculate a len stepLeft, b)) [0..] filtered
       points = map (\(a,b) -> b!!a) coord
   in map ( == '#') points



-- step to the right based on list index
calculate index modulo step = mod (index * step) modulo

-- Filter List, return every "step" line
filterList step list = map snd $ filter (\(a,_)-> mod a step == 0) $ zip [0..] list

-- [Bool] -> Int where true
trees a = length [e | e<-a, e]