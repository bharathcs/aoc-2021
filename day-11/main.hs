import Data.List (findIndices, (\\), elemIndex)

main :: IO ()
main = do
    let sample = parse "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526\n"
    fileContents <- readFile "input.txt"
    let streamOfDays = iterate runOneDay $ parse fileContents
    let numFlashes = length $ concatMap (concatMap (filter (== 0))) $ take 101 streamOfDays
    print $ "Count of flashes: " ++ show numFlashes
    let dDay = elemIndex ([ take 10 $ repeat 0 | _ <- [0..9]]) streamOfDays
    print $ "Day they all flash: " ++ show dDay

-- utils

type Octopus = Int
type Cavern = [[Octopus]]
type Position = (Int, Int)
type Event = Cavern -> Cavern

parse :: String -> Cavern
parse = map (map (read . (:""))) . lines

infinite2dPlain :: [[Position]]
infinite2dPlain = map (\x -> map (\y -> (x,y)) [0..]) [0..]
blankCavern = take 10 $ map (take 10) infinite2dPlain

isWithinBounds :: Position -> Bool
isWithinBounds (x,y) = and [x >= 0, y >= 0, x < 10, y < 10]

iterateFn n fn x
    | n < 0 = error "Negative number"
    | n == 0 = x
    | n == 1 = fn x
    | otherwise = fn (iterateFn (n-1) fn x)

-- logic

runOneDay :: Cavern -> Cavern
runOneDay = resetFlashed . flash . incr

incr :: Event
incr = map (map (+1))
flash :: Event
flash c = executeFlash c [] (findFlash c)
resetFlashed :: Event
resetFlashed = map (map (\x -> if x >= 10 then 0 else x))

-- helpers to event

findFlash :: Cavern -> [Position]
findFlash cavern = addRowCoordinatesAndFlatten flashesIn2D
    where
        flashesIn2D = map (findIndices (>=10)) cavern
        addRowCoordinatesAndFlatten = concat . zipWith (\x ys -> map (\y -> (x,y)) ys) [0..]
executeFlash :: Cavern -> [Position] -> [Position] -> Cavern
executeFlash cavern flashed flashingNow
    | null flashingNow = cavern
    | otherwise = executeFlash newCavern (flashingNow ++ flashed) nextToFlash
    where
        addAllDir point = filter (/= point) . concatMap (\(x,y) -> [(x,y-1), (x,y), (x,y+1)]) . (\(x,y) -> [(x-1,y),(x,y),(x+1,y)]) $ point
        filterFlashedOrInvalid = filter isWithinBounds . ( \\ flashingNow) . ( \\ flashed)
        convertPointsToMask (x, y) = map (map (\(a,b) -> if a == x && b == y then 1 else 0::Int)) blankCavern
        addMask = zipWith (zipWith (+)) :: Cavern -> Cavern -> Cavern
        listOfFlashingNowMasks = map convertPointsToMask $ filterFlashedOrInvalid $ concatMap addAllDir flashingNow
        newCavern = foldl addMask cavern listOfFlashingNowMasks
        nextToFlash = filterFlashedOrInvalid $ findFlash newCavern
