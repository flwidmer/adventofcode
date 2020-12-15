{-# LANGUAGE TupleSections #-}
import Data.List.Split ( splitOn )
import qualified Data.Map as Map

main :: IO ()
main = do
    input <- readFile "input"
    print "day 14"
    print $ solve1 input
    print $ solve2 input

solve1 :: String -> Int
solve1 input =
    let program = parse input
        memory = recurse1 program "" Map.empty
    in sum $ map backToInt $ Map.elems memory

solve2 :: String -> Int
solve2 input =
    let program = parse input
        memory = recurse2 program "" Map.empty
    in sum $ Map.elems memory

recurse1 :: [(String, String, Int, Int)] -> String -> Map.Map Int String -> Map.Map Int String
recurse1 [] _ m = m
recurse1 (("mask", newMask, _,_):ll) _ m = recurse1 ll newMask m
recurse1 (("mem", _, addr, val):ll) msk m =
    let modified = concatMap show $ modify val msk
        newMap = Map.insert addr modified m
    in recurse1 ll msk newMap

recurse2 :: [(String, String, Int, Int)] -> String -> Map.Map Int Int -> Map.Map Int Int
recurse2 [] _ m = m
recurse2 (("mask", newMask, _,_):ll) _ m = recurse2 ll newMask m
recurse2 (("mem", _, addr, val):ll) msk m =
    let modified = modifyFloating msk addr
        insertions = map (\x -> (x, val)) modified
        newMap = foldl insertion m insertions
    in recurse2 ll msk newMap
    where insertion mm (a,v) = Map.insert a v mm

parse input =
    let l = lines input
        instructions = map extractInstruction l
    in instructions

extractInstruction i =
    let [left, right] = splitOn " = " i
    in parseInstr left right

parseInstr "mask" r = ("mask", r, 0,0)
parseInstr l r =
    let addr = read $ filter isDigit l
        val = read r
    in ("mem", "", addr, val)
    where isDigit x = x `elem` "0123456789"

bin :: [Int] -> Int
bin a =
  let len = length a
      mult = reverse $ map (2^) [0..len-1]
  in  sum $ zipWith (*) mult a


modify :: Int -> String -> [Int]
modify val msk =
    let bin = pad 36 $ toBinary val
    in zipWith (curry flipOrNot) msk bin
    where
        flipOrNot ('1', _) = 1
        flipOrNot ('0', _) = 0
        flipOrNot (_, x) = x

modify2 :: Int -> String -> String
modify2 val msk =
    let bin = pad 36 $ toBinary val
    in  zipWith (curry flipOrNot) msk bin
    where
        flipOrNot ('1', _) = '1'
        flipOrNot ('0', 1) = '1'
        flipOrNot ('0', 0) = '0'
        flipOrNot ('X', _) = 'X'

modifyFloating :: String -> Int -> [Int]
modifyFloating msk addr =
    let modifiedMask = modify2 addr msk
        floatingMasks = floating modifiedMask ""
        -- floatingAddresses = map (modify2 addr) floatingMasks
    in map backToInt floatingMasks

floating :: String -> String -> [String]
floating "" result = [result]
floating remain result =
    let current = head remain
        rest = tail remain
    in if current == 'X'
        then floating rest (result++ "1") ++ floating rest (result++ "0")
        else floating rest (result ++ [current])

digitToInt :: Num p => Char -> p
digitToInt '1' = 1
digitToInt '0' = 0
digitToInt _ = error "weird"

backToInt :: String -> Int
backToInt str = bin $ map digitToInt str

intBackToInt :: [Int] -> Int
intBackToInt str = bin $ map digitToInt $ concatMap show str

pad :: Num a => Int -> [a] -> [a]
pad len l = replicate (len - length l) 0 ++ l

toBinary 0 = [0]
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]