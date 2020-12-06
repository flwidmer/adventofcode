import Data.List.Split 

main :: IO ()
main = do 
        input <- readFile "input"
        print $ solve1 $ lines input
        print $ solve2 $ lines input

solve1 :: [String] -> Int
solve1 a = 
  let passwords = map parseLine a
      validPasswords = filter checkLine1 passwords
  in  length validPasswords

solve2 :: [String] -> Int
solve2 a = 
  let passwords = map parseLine a
      validPasswords = filter checkLine2 passwords
  in  length validPasswords

checkLine2 :: ((Int, Int), Char, String) -> Bool
checkLine2 ((lowerPos, upperPos), character, password) = 
  let pos1 = (password !! (lowerPos - 1)) == character 
      pos2 = (password !! (upperPos - 1)) == character 
  in  pos1 `xor` pos2

checkLine1 :: ((Int, Int), Char, String) -> Bool
checkLine1 ((lowerLimit, upperLimit), character, password) = 
  let count = length $ filter (character ==) password
  in (count >= lowerLimit) && (count <= upperLimit)

parseLine :: String -> ((Int, Int), Char, String)
parseLine a = 
  let w = words a
      limits = parseLimits $ splitOn "-" $ head w 
      character = head (w !! 1)
      password = w !! 2 
  in (limits , character, password)

parseLimits :: [String] -> (Int, Int)
parseLimits [a,b] = (read a, read b)

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)