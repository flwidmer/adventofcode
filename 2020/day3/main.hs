main = do 
        input <- readFile "input"
        -- solve $ inputList input
        putStrLn $ show $ trees $ solve 1 3 $ inputList input
        putStrLn $ show $ solve2 input
        putStrLn "xxx"
    
solve2 inp = 
   let  input = inputList inp
        one = trees (solve 1 1 $ input )
        two = trees (solve 1 3 $ input)
        three = trees (solve 1 5 $ input)
        four = trees (solve 1 7 $ input)
        five = trees (solve 2 1 $ input)
    in one * two * three * four * five

solve stepDown stepLeft l = 
   let len = length $ head l
       filtered = filterList stepDown l
       coord = map (\(a,b) -> (calculate a len stepLeft, b) ) (zip [0..] filtered) 
       points = map (\(a,b) -> b!!a) coord 
   in map (\a -> a == '#') points

inputList input = lines input

-- step to the right based on list index
calculate index modulo step = mod (index * step) modulo

-- Filter List, return every "step" line
filterList step list = map snd $ filter (\(a,b)-> mod a step == 0) $ zip [0..] list

-- [Bool] -> Int where true
trees a = length $ [e | e<-a, e]