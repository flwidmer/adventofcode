import Data.List

main :: IO ()
main = do
    input <- readFile "input"
    print "day8"
    print $ solve1 input
    print $ solve2 input

solve1 :: String -> Int
solve1 input =
    let program = map parseInstructions $ lines input
        execution = run program (0,0)
    in  fst $ last $ take (length (executeUntilDuplicate program)) execution

solve2 :: String -> Int
solve2 input =
    let program = map parseInstructions $ lines input
        modifiedPrograms = map (modifyProgram program) [0.. length program -1]
        aGoodOne = filter (\(a,b) -> (-1) `elem` executeUntilDuplicate b) modifiedPrograms
        goodProgram = modifiedPrograms !! fst (head aGoodOne)
        acc = takeWhile (\(_,x) -> x /= (-1)) $ run (snd goodProgram) (0,0)
    in fst $ last acc

executeUntilDuplicate :: [(String, Int)] -> [Int]
executeUntilDuplicate program =
    let execution = run program (0,0)
    in findLoop [] $ map snd execution

parseInstructions :: String -> (String, Int)
parseInstructions w = pack $ words w
    where pack x = (head x, read $ filter ( /= '+') (x!!1))

run :: [(String, Int)] -> (Int, Int) -> [(Int, Int)]
run program state =
    let current = executeLine program state
        next = run program current
    in current : next


executeLine :: [(String, Int)] -> (Int, Int) -> (Int, Int)
executeLine _ (accumulator, -1) = (accumulator, -1)
executeLine program (accumulator, line)
    | line < length program =
        let currentLine = program !! line
            currentAccumulator = execAcc accumulator currentLine
            nextLine = execJmp line currentLine
        in (currentAccumulator, nextLine)
    | otherwise = (accumulator, -1)

execAcc :: Int -> (String, Int) -> Int
execAcc accumulator ("acc", inc) = accumulator + inc
execAcc accumulator (_, _) = accumulator

execJmp :: Int -> (String, Int) -> Int
execJmp line ("jmp", num) = line+ num
execJmp line (_,_) = line+ 1

findLoop :: [Int] -> [Int] -> [Int]
findLoop seen notSeen
    | null notSeen = []
    | null seen = findLoop [head notSeen] (drop 1 notSeen)
    | head notSeen `elem` seen = reverse seen
    | otherwise = findLoop (head notSeen : seen) (drop 1 notSeen)

modifyProgram :: [(String, Int)] -> Int -> (Int, [(String, Int)])
modifyProgram program line
    | "jmp" == fst (program !! line) = (line, set "nop" program line)
    | "nop" == fst (program !! line) = (line, set "jmp" program line)
    | otherwise = (line, program)

set instr program line =
    let (_, val) = program !! line
        newElement = [(instr, val)]
    in take line program ++ newElement ++ drop (line + 1) program