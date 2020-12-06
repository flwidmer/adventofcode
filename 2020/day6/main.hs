import Data.List.Split ( splitOn )
import Data.List ( intersect, nub )

main :: IO ()
main = do
    input <- readFile "input"
    putStrLn "day6"
    print $ solve1 input
    print $ solve2 input
    
solve1 :: String -> Int
solve1 a = 
  let grouped = groups a
      answers = map (filter (\x -> x `elem` ['a'..'z']) . nub) grouped
  in  addAll answers

solve2 :: String -> Int
solve2 a =
  let grouped = map lines $ groups a
      intersected = map (foldr1 intersect) grouped 
  in  addAll intersected

addAll :: [[a]] -> Int 
addAll = sum . map length 

groups :: String -> [String]
groups = splitOn "\n\n"

