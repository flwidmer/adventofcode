import Data.List.Split
import Data.Char (isDigit)
import qualified Data.Bifunctor as BF
import Data.Function (on)
import Data.List (sortBy)


width = 10

main = do
    input <- readFile "input"
    print "day 20"
    print $ solve1 input
    



newtype Border = Border String deriving (Show, Eq)

data Tile = Tile {
    num :: Int,
    image :: String,
    left :: Border ,
    right :: Border ,
    top :: Border ,
    bottom :: Border
} deriving (Show)


solve1 input =
    let tiles = readTiles input
        border_num =concatMap extractBorders tiles
        border_matches = map (\x -> (snd x, lookupAllValues border_num $ fst x)) border_num
        borderid_numberOfMatches = map (BF.second ((\x -> x-1) . length)) border_matches
        tileIds = map num tiles
        tileid_numberOfMatches = map (\x -> (x , lookupAndSumSnd borderid_numberOfMatches x )) tileIds
        corners = take 4 $ sortBy (compare `on` snd) tileid_numberOfMatches
    in product $ map fst corners 
    

lookupAllValues :: Eq a => [(a, b)] -> a -> [(a, b)]
lookupAllValues aList val = filter (\(a,_) -> a == val) aList

lookupAndSumSnd aList val = sum $ map snd  $ lookupAllValues aList val

extractBorders :: Tile -> [(Border, Int)]
extractBorders tile =
    let n = num tile
        rL = rev $ left tile 
        rR = rev $ right tile
        rB = rev $ bottom tile
        rT = rev $ top tile   
    in [(left tile ,n),(right tile ,n),(top tile ,n),(bottom tile,n), (rL, n), (rR, n), (rB, n), (rT, n)]

rev (Border str) = Border (reverse str)

readTiles :: String -> [Tile]
readTiles input =
    let tiles = splitOn "\n\n" input
    in map readTile tiles

readTile :: String -> Tile
readTile input =
    let l = lines input
        name = head l
        num = read $ filter isDigit name
        im = concat $ tail l
        top = Border $ take width im
        bottom =Border $ drop (length im - width) im
        left = Border $ takeEvery 0 im
        right = Border $ takeEvery (width - 1) im
    in Tile {num = num, left = left, top = top, right = right, bottom = bottom, image = im}
    where takeEvery n from = map snd $ filter (\(a,_) -> a `mod` width == n) $ zip [0..] from



