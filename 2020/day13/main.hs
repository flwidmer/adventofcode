import Data.List.Split

main = do
    input <- readFile "input"
    print "day 13"
    print $ solve1 input 
    print $ solve2crt input 
    

solve1 input =
    let l = lines input 
        startTime = readInt $ head l
        busLines = map readInt $ filter (/= "x") $ splitOn ","  $ last l
        deps = map (findNextDeparture startTime) busLines
        nextDeps = map (\(a,b) -> (a+b, b)) deps
        best = minimum $ map fst nextDeps
        busLine = snd $ head $ filter (\(a,b) -> a == best) nextDeps
    in (best - startTime) * busLine


findNextDeparture startTime freq = 
    let nextDepartures = map (*freq) [0..]
        best = last $ takeWhile (< startTime) nextDepartures
    in (best, freq)

readInt :: String -> Int 
readInt = read

-- brute force
solve2 = 
    let times = [100000000000000..]
        func = map calc [(37,0),(41,27),(457,37),(13,50),(17,51),(23,60),(29,66),(431,68),(19,87)]
        exec = map (appl func) times
    in 100000000000000 + (length $ takeWhile (\x -> (foldl1 (&&) x) /= True) exec)
    where appl func x =  map ($ x) func
-- [("37",0),("41",27),("457",37),("13",50),("17",51),("23",60),("29",66),("431",68),("19",87)]
--1068781

calc (a,b) x = mod (x + b)  a == 0

calc2test = 
    let times = [1000..]
        func = map calc [(7,0),(13,1),(59,4),(31,6),(19, 7)]
        exec = map (appl func) times
    in 1000 + (length $ takeWhile (\x -> (foldl1 (&&) x) /= True) exec)
    where appl func x =  map ($ x) func
    

solve2crt input =
    let l = map (\(a,b) -> (a, readInt b))$filter (\(a,b) -> b /= "x") $ zip [0..] $ splitOn "," $ last $ lines input
        flipped = map (\(a,b) -> (b-a, b) ) l
    in flipped

-- [(0,37), (14,41),(420,457),(-37,13),(-34,17),(-37,23),(-37,29),(363,431),(-68,19)]
-- ok, I got this off Stackoverflow...
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
    where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1
    -- Modular Inverse
    a `inv` m = 
        let (_, i, _) = gcd a m 
        in i `mod` m
    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
        where (g, s, t) = gcd (b `mod` a) a