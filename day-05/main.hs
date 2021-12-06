import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    -- let sample = parse "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2\n"

    fileContents <- readFile "input.txt"
    let points = parse fileContents
    print $ length $ analyse points

-- Utils

type Point = (Int, Int)
type Line = (Point, Point)

parse :: String -> [Line]
parse = map parseLine . lines

parseLine :: String -> Line
parseLine = (\ (x:"->":y:_) -> (parsePoint x, parsePoint y)) . words

parsePoint :: String -> Point
parsePoint input = (read firstInt, read secondInt)
    where (firstInt, _comma:secondInt) = break (== ',') input

storeMap :: [Point] -> Map Point Int
storeMap = foldl insertIntoMap Map.empty
lookupElse0 :: Point -> Map Point Int -> Int
lookupElse0 p currMap = fromMaybe 0 (Map.lookup p currMap)
insertIntoMap :: Map Point Int -> Point -> Map Point Int
insertIntoMap currMap p = Map.insert p (lookupElse0 p currMap + 1) currMap

-- Part One

getPointsFromLine :: Line -> [Point]
getPointsFromLine ((a, b), (c, d))
    | (a > c && b == d) || (a == c && b > d) = getPointsFromLine ((c, d), (a, b))
    | a == c && b == d = [(a, b)]
    | a == c && b + 1 <= d = (a, b) : getPointsFromLine ((a, b + 1), (c, d))
    | b == d && a + 1 <= c = (a, b) : getPointsFromLine ((a + 1, b), (c, d))
    | otherwise = []

getAllPointsFromLines :: [Line] -> [Point]
getAllPointsFromLines = concatMap getPointsFromLine

analyse :: [Line] -> [Point]
analyse arr = Map.keys mapOfDangerousPoints
    where   m = storeMap $ getAllPointsFromLines arr
            mapOfDangerousPoints = Map.filter (>= 2) m
