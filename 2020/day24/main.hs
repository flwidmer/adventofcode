import qualified Data.Map as M
import Debug.Trace (trace)


main :: IO ()
main = do
    input <- readFile "input"
    print "day 24"
    print $ solve1 input
    print $ solve2 input

solve1 input =
    let ll = lines input
        paths = map consumeLine ll
    in sum $ M.elems $ doFlips M.empty paths

solve2 input =
    let ll = lines input
        paths = map consumeLine ll
        startFloor = fill $ doFlips M.empty paths
    in sum $ M.elems $ generation 100 startFloor

generation 0  floor = floor
generation n floor =
    let nextFloor = M.mapWithKey (determineNextState floor) floor
        filledNextFloor = fill nextFloor
    in trace ("generation " ++ show n ++ " number " ++ show (sum $ M.elems nextFloor)) generation (n-1) filledNextFloor


fill::  M.Map (Int, Int) Int ->  M.Map (Int, Int) Int
fill floor =
    let k = M.keys floor
        x = map fst k
        y = map snd k
        minX = minimum x
        maxX = maximum x
        minY = minimum y
        maxY = maximum y
        missing = [((x,y), 0) | x <- [minX-1..maxX+1], y <- [minY-1..maxY+1], (x,y) `M.notMember` floor]
        missingMap = M.fromList missing
    in M.union floor missingMap

determineNextState :: M.Map (Int, Int) Int -> (Int, Int) -> Int -> Int
determineNextState floor tile state =
    let neighborCount = countNeighbors floor tile
    in nextState state neighborCount
    where nextState state num 
            | num > 2 = 0
            | num == 0 = 0
            | num == 2 = 1
            | otherwise = state

countNeighbors :: M.Map (Int, Int) Int -> (Int, Int) -> Int
countNeighbors floor tile =
    let sideSteps = map (`step` tile) neighbors
        numberOfNeighbors = sum $ map (\x -> M.findWithDefault 0 x floor) sideSteps
    in numberOfNeighbors

neighbors = [(1,-1),(1,1),(-1,1),(-1,-1),(-2,0),(2,0)]

doFlips floor [] = floor
doFlips floor (p:paths) =
    let toFLip = walk p
        newFloor = M.alter flipTile toFLip floor
    in doFlips newFloor paths

flipTile (Just 0) = Just 1
flipTile (Just 1) = Just 0
flipTile Nothing = Just 1
flipTile _ = error "what are you doing?"

walk :: [(Int, Int)] -> (Int, Int)
walk = foldl step (0,0)

step :: (Int, Int) -> (Int, Int) -> (Int, Int)
step (dx,dy) (px,py) = (px+dx, py+dy)

consumeLine :: String -> [(Int, Int)]
consumeLine ('s':'e':xs) = (1,-1):consumeLine xs
consumeLine ('s':'w':xs) = (-1,-1):consumeLine xs
consumeLine ('n':'e':xs) =(1,1):consumeLine xs
consumeLine ('n':'w':xs) =(-1,1):consumeLine xs
consumeLine ('w':xs) =(-2,0):consumeLine xs
consumeLine ('e':xs) =(2,0):consumeLine xs
consumeLine [] = []

