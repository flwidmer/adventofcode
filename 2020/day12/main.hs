main = do
    input <- readFile "input"
    print "day12"
    print $ solve1 input  
    print $ solve2 input 

solve1 ::String -> Int
solve1 input =
    let instr = map parseInput $ lines input
        init = ('E',0,0)
        (_, r1,r2) = foldl move init instr
    in abs r1 + abs r2

solve2 ::String -> Int
solve2 input =
    let instr = map parseInput $ lines input
        init = (0,0, 10,1)
        (r1, r2, _,_) = foldl move2 init instr
    in abs r1 + abs r2

move :: (Char, Int, Int) -> (Char, Int) -> (Char, Int, Int)
move (direction, currentX, currentY) (instruction, number)
    | instruction == 'N' = (direction, currentX + number, currentY)
    | instruction == 'S' = (direction, currentX - number, currentY)
    | instruction == 'E' = (direction, currentX, currentY + number)
    | instruction == 'W' = (direction, currentX, currentY - number)
    | instruction == 'F' = move (direction, currentX, currentY) (direction, number)
    | otherwise = (turn instruction number direction, currentX, currentY)

move2 :: (Int, Int,Int, Int) -> (Char, Int) -> (Int, Int, Int, Int)
move2 (currentX, currentY, waypointX, waypointY ) (instruction, number)
    | instruction == 'N' = (currentX, currentY, waypointX, waypointY + number)
    | instruction == 'S' = (currentX, currentY, waypointX, waypointY - number)
    | instruction == 'E' = (currentX, currentY, waypointX + number, waypointY)
    | instruction == 'W' = (currentX, currentY, waypointX - number, waypointY)
    | instruction == 'F' = (currentX + number * waypointX, currentY + number * waypointY, waypointX, waypointY)
    | otherwise =
        let turnedWp = turnWp instruction number waypointX waypointY
        in (currentX, currentY, fst turnedWp, snd turnedWp)

turn :: Char -> Int -> Char -> Char
turn instruction number currentDirection =
    let degree = direction instruction (currentDegree currentDirection) number
    in  compass $ mod degree 360

turnWp :: Char -> Int -> Int -> Int -> (Int, Int)
turnWp instruction number wpX wpY = 
    let movement = direction instruction 0 number
        pointing = mod movement 360
    in rotateWp pointing wpX wpY

rotateWp :: Int -> Int -> Int -> (Int, Int)
rotateWp 90 wpX wpY = (wpY, (-1) * wpX)
rotateWp 180 wpX wpY = ((-1) * wpX, (-1) * wpY)
rotateWp 270 wpX wpY = ((-1) * wpY, wpX)
rotateWp 0 wpX wpY = (wpX, wpY)
    
currentDegree :: Char -> Int
currentDegree 'N' = 0
currentDegree 'S' = 180
currentDegree 'W' = 270
currentDegree 'E' = 90

compass :: Int -> Char
compass  0 = 'N'
compass  180 = 'S'
compass  270 = 'W'
compass 90 = 'E'

direction :: Char -> Int -> Int -> Int
direction 'L' = (-)
direction 'R' = (+)

parseInput :: String -> (Char, Int)
parseInput l = (head l, read $ tail l)

