
import Data.List.Split (splitOn)
import Data.List (intercalate, sortBy, intersect, nub, (\\) )
import Debug.Trace (trace)
import qualified  Data.Bifunctor as BF
import Data.Function (on)

main = do
    input <- readFile "input"
    print "day 21"
    print $ solve1 input
    print $ solve2 input

dbg = False
debug a b = if dbg then trace a b else b

solve1 input =
    let safeIngredients = safe input
    in length $ filter ( `elem` safeIngredients) $ words input


solve2 input =
    let safeIngredients = safe input
        allergen_ingredient = parseInput1 input
        allergen_unsafeIngredients = map (removeSafe safeIngredients) allergen_ingredient
        allAllergens = nub $ map fst allergen_ingredient
        candidates_translation = recurse2 [] allAllergens allergen_unsafeIngredients
        ordered = sortBy (compare `on` fst) candidates_translation
    in intercalate "," $ map snd ordered


recurse2 :: [(String, String)] -> [String] -> [(String, [String])] -> [(String, String)]
recurse2 resolved [] _ = resolved -- base case
recurse2 resolved unresolvedIngredients assocList =
    let values = nub $ map (resolve assocList) unresolvedIngredients
        resolvedThisStep = nub $ map (BF.second head) $ filter (\x -> length (snd x) == 1) values
        unresolved = filter (`notElem` map fst resolvedThisStep) unresolvedIngredients
        resolvedTranslationsThisStep = map snd resolvedThisStep
        aListMinusResolved = filter (\x -> fst x `notElem` map fst resolvedThisStep) assocList
        removeResolvedFromLists = map (BF.second (filter (`notElem` resolvedTranslationsThisStep))) aListMinusResolved
    in debug ("this step: " ++ show resolvedThisStep)  recurse2 (resolvedThisStep ++ resolved) unresolved removeResolvedFromLists

resolve :: [(String, [String])] -> String -> (String, [String])
resolve allergen_unsafeIngredients value =
    let translations = foldr1 intersect $ map snd $  lookupAllValues allergen_unsafeIngredients value
    in debug ("resolve " ++ value ++ " " ++ show translations) (value, translations)


removeSafe :: [String] -> (String, [String]) -> (String, [String])
removeSafe safeList (allergen, ingredients) =
    let cleanList = filter (`notElem` safeList) ingredients
    in (allergen, cleanList)

safe input =
    let allergen_ingredient = parseInput1 input
        allergens = map fst allergen_ingredient
        candidateAllergens = nub $ concatMap (intersectIngredients allergen_ingredient) allergens
        allingredients = concatMap snd allergen_ingredient
        ingredients = nub allingredients
        safeIngredients = ingredients \\ candidateAllergens
    in safeIngredients

intersectIngredients aList key =
    let containedIn = map snd $ lookupAllValues aList key
    in foldr1 intersect containedIn

lookupAllValues :: Eq a => [(a, b)] -> a -> [(a, b)]
lookupAllValues aList val = filter (\(a,_) -> a == val) aList

parseInput1 input =
    let l = lines input
    in concatMap parseLine l

parseLine l =
    let (ing:all:_) = splitOn "(contains " l
        allergens = words $ filter (/= ',') $ filter (/= ')') all
        ingredients = words ing
    in map (\x -> (x,ingredients)) allergens

