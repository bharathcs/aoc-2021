import Data.Char (isUpper, isLower)
import Data.List (sort, group)
main :: IO ()
main = do
    let sample1 = parse "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n"
    let sample2 = parse "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc\n"
    fileContents <- readFile "input.txt"

    -- let sample = parse fileContents
    let sample = sample1
    -- print sample
    -- print $ dfs sample []
    print $ length $ dfs sample []
    -- print $ dfs sample []
    print $ length $ dfs2 sample []


-- utils

type Cave = String
type Path = (Cave, Cave)

parse :: String -> [Path]
parse = dontBacktrack . parseLine . lines
    where
        dontBacktrack = filter (\(x,y) -> y /= "start" && x /= "end")
        parseLine = concatMap ((\(x,_:y) -> [(x,y),(y,x)]) . span (/= '-'))

-- part 1

dfs :: [Path] -> [Cave] -> [[Cave]]
dfs paths trail@(curr:past)
    | curr == "end" = [reverse trail]
    | otherwise = concat [ dfs paths (next:curr:past) | (from, next) <- paths, from == curr, all isUpper next || next `notElem` past]
dfs paths [] = dfs paths ["start"]

-- part 2

checkValidTrail2 :: [Cave] -> Cave -> Bool
checkValidTrail2 validTrail next = all isUpper next || (isValidNumOfVisits . getNumberOfVisitsToEachSmallCave) (next:validTrail)
    where
        getNumberOfVisitsToEachSmallCave = map length . group . sort . filter (all isLower)
        isValidNumOfVisits xs = filter (/= 1) xs `elem` [[], [2]] -- any number of 1s, but at most 1 2.


dfs2 :: [Path] -> [Cave] -> [[Cave]]
dfs2 paths trail@(curr:past)
    | curr == "end" = [reverse trail]
    | otherwise = concat [ dfs paths (next:curr:past) | (from, next) <- paths, from == curr, checkValidTrail2 trail next]
dfs2 paths [] = dfs paths ["start"]