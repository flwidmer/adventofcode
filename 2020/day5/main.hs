import Data.List

main = do
  input <- readFile "input" 
  putStrLn "day5"
  putStrLn . show . solve1 $ lines input
  putStrLn . show . solve2 $ lines input

-- input format
-- BFFFBBFRRR
-- B is back, F is front. binary encoding.


-- find maximum seat number
solve1 :: [String] -> Integer
solve1 a = maximum $ map seatnum a 

-- sort the seats, take the lowest number as offset and start comparing with zip
-- filter all the ones where the seatnumber does not equal the offset index
solve2 :: [String] -> Integer 
solve2 a = 
  let seats = sort $ map seatnum a 
      firstSeat = minimum seats 
  in fst . head . filter offsetIndexNotEqual $ zip [firstSeat..] seats 
  where offsetIndexNotEqual (i,k) = i /= k
  
-- calculate seatnumber according to rule 
seatnum :: String -> Integer
seatnum a = 
  let (row, seat) = separate 7 a
      x = bin $ map transBF row
      y = bin $ map transRL seat
  in x * 8 + y
  where
    transBF = translate 'B' 'F'
    transRL = translate 'R' 'L'
    

-- separate the input
separate :: Int -> String -> (String, String)
separate amount list = (take amount list, drop amount list)

-- translate a given input to binary
translate :: Char -> Char -> Char -> Integer
translate one zero inp
  | one == inp = 1
  | zero == inp = 0
  | otherwise = -1

-- calculate the decimal value of a binary input such as [1,0,0,1,1]
bin :: [Integer] -> Integer
bin a =
  let len = length a
      mult = reverse $ map (2^) [0..len-1]
  in  sum $ map (\(a,b) -> a * b) $ zip mult a
