import Debug.Trace

main = do
    input <- readFile "test"
    print "day11"
    print $ length $ filter ('#' ==) $ solve1 input

solve1 :: String -> String
solve1 input =
    let matrix = lines input
        width = length $ head matrix
        state = concat matrix
    in recurse width state 
        
recurse :: Int -> String -> String
recurse width state =
    let next = nextState width state
        x = debug (show next) 1
    in if state == next then state else recurse width next

nextState :: Int -> String -> String
nextState width state =
    let positions = zip state [0..]
        next = map (nextStateSingle width positions) positions
    in next

nextStateSingle :: Int -> [(Char, b)] -> (Char, Int) -> Char
nextStateSingle width positions currentPosition =
    let surroundingPs = surroundingPositions (length positions) width $ snd currentPosition 
        countOccupied = length $ filter ('#' ==) $ map (getSymbol positions) surroundingPs
    in calc (fst currentPosition) countOccupied
    where 
        calc '#' countOccupied = if countOccupied >= 4 then 'L' else '#'
        calc 'L' countOccupied = if countOccupied == 0 then '#' else 'L'
        calc '.' _ = '.'

surroundingPositions :: Int -> Int -> Int -> [Maybe Int]
surroundingPositions len width position = 
    -- using compass directions 
    let n =  if up            then Just (position - width)     else Nothing -- go one row back
        ne = if up && right   then Just (position - width + 1) else Nothing -- go back one row, add 1
        e =  if right         then Just (position + 1)         else Nothing -- go one right
        se = if down && right then Just (position + width + 1) else Nothing -- go one row ahead, one right
        s =  if down          then Just (position + width)     else Nothing -- go one row ahead, one right
        sw = if down && left  then Just (position + width -1)  else Nothing
        w =  if left          then Just (position - 1)         else Nothing
        nw = if up && left    then Just (position - width - 1) else Nothing
    in [n, ne,e, se, s, sw, w, nw]
    where 
        up =    position - width > 0
        left =  mod position width /= 0
        right = mod (position + 1) width /= 0
        down =  position + width < len

getSymbol :: [(Char, b)] -> Maybe Int -> Char
getSymbol positions (Just query) = fst $ positions !! query
getSymbol _ Nothing = '-'

debug :: String -> a -> a
debug = trace
-- debug _ a = a

