import Data.List.Split
import Data.Char (isDigit)
import qualified Data.Bifunctor as BF
import Data.Function (on)
import Data.List (sortBy, intercalate)
import Debug.Trace ( trace )

width = 10
imageWidth = 12
imagePixels = 12 * 8

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
    bottom :: Border,
    borderless :: String
} deriving (Show)

solve1 :: String -> Int
solve1 input =
    let tiles = readTiles input
        border_num =concatMap extractBorders tiles
        border_matches = map (\x -> (snd x, lookupAllValues border_num $ fst x)) border_num
        borderid_numberOfMatches = map (BF.second ((\x -> x-1) . length)) border_matches
        tileIds = map num tiles
        tileid_numberOfMatches = map (\x -> (x , lookupAndSumSnd borderid_numberOfMatches x )) tileIds
        corners = take 4 $ sortBy (compare `on` snd) tileid_numberOfMatches
    in product $ map fst corners

solve2 input = 
    let tiles = readTiles input
        border_num =concatMap extractBorders tiles
    in border_num

recurse allTiles assembledPieces currentPiece 
    | length currentPiece < 12 = 
        -- add one to the right of the current piece.
        -- matching using right / left. Maybe to as well?
        -- remove that one from allTiles
        -- continue recursing
    | otherwise 
        -- move currentPiece at end of assembled
        -- find a matching top for the first one of the last top
        -- remove that from allTiles 
        -- continue recursing



lookupAllValues :: Eq a => [(a, b)] -> a -> [(a, b)]
lookupAllValues aList val = filter (\(a,_) -> a == val) aList

lookupAndSumSnd ::  [(Int, Int)] -> Int -> Int
lookupAndSumSnd aList val = sum $ map snd  $ lookupAllValues aList val

extractBorders :: Tile -> [(Border, Int)]
extractBorders tile =
    let n = num tile
        rL = rev $ left tile -- flip the border to 
        rR = rev $ right tile
        rB = rev $ bottom tile
        rT = rev $ top tile
    in [(left tile ,n),(right tile ,n),(top tile ,n),(bottom tile,n), (rL, n), (rR, n), (rB, n), (rT, n)]

rev :: Border -> Border
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
        imWithoutBorder = concat $ take (width -2) $ tail $ map dropBorder $ tail l
    in Tile {num = num, left = left, top = top, right = right, bottom = bottom, image = im, borderless = imWithoutBorder}
    where
        takeEvery n from = map snd $ filter (\(a,_) -> a `mod` width == n) $ zip [0..] from
        dropBorder s = tail $ take (width - 1) s


rotateRightTile t =
    let ol = left t
        or = right t
        ot = top t
        ob = bottom t
        ri = rotateRightImage width $ image t
        rbi = rotateRightImage (width -2) $ borderless t
    in Tile {num = num t, left = ob, top = ol, right = ot, bottom = or, image = ri, borderless = rbi}

rotateRightImage w img =
    let l = chunksOf w img
    in concatMap (reverse . slice l) [0..w -1]

slice :: [String] -> Int -> String
slice img n = map (!! n) img


printTile :: Tile -> String
printTile t =
    let l = chunksOf (width -2 ) $ borderless t
    in intercalate "\n" l

debug t = trace t