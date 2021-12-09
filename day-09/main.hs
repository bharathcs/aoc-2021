import Data.List (sort)
main :: IO ()
main = do
    let sample = parse "2199943210\n3987894921\n9856789892\n8767896789\n9899965678\n"
    fileContents <- readFile "input.txt"
    print $ getTotalRisk $ parse fileContents
    let res = sort $ map length $ getBasins $ parse fileContents
    print $ product (drop (length res - 3) res)

-- utils

type HeatMap = [[Int]]
type Pos = (Int, Int)

parse :: String -> HeatMap
parse input = map (map (read . (:[]))) $ lines input

addPos :: Pos -> Pos -> Pos
addPos p q = (fst p + fst q, snd p + snd q)

getAdjPos :: Pos -> [Pos]
getAdjPos input = map (addPos input) lrud
    where lrud = [(1,0), (-1,0), (0,1), (0, -1)] -- left right up down

isValidPos :: Int -> Int -> Pos -> Bool
isValidPos maxX maxY (x, y) = x >= 0 && y >= 0 && x < maxX && y < maxY

getHeight :: HeatMap -> Pos -> Int
getHeight map (x, y)
    | isValidPos a b (x,y) = map !! x !! y
    | otherwise = error $ "Pos " ++ show (x,y) ++ " not in " ++ show (a,b)
    where (a, b) = getDimensions map

getDimensions :: HeatMap -> (Int, Int)
getDimensions xss = (length xss, length (head xss))

getList :: Int -> Int -> [Pos]
getList maxX maxY = foldl (\prev curr -> prev ++ curr `with` [0..(maxY-1)]) [] [0..(maxX-1)]
    where
        x `with` (y:ys) = (x, y): x `with` ys
        x `with` [] = []

-- Part One

getLowPoints :: HeatMap -> [Pos]
getLowPoints hm = filter (\ p -> isLowPoint (getRealAdjPoints p) (getHeight hm p)) points
    where
        (maxX, maxY) = getDimensions hm
        points = getList maxX maxY
        heights = map (getHeight hm) points
        getRealAdjPoints = filter (isValidPos maxX maxY) . getAdjPos
        isLowPoint xs i = all (\x -> getHeight hm x > i) xs

getTotalRisk :: HeatMap -> Int
getTotalRisk hm = sum $ map ((+ 1) . getHeight hm) (getLowPoints hm)

type IntermediateBasin = (HeatMap, [Pos], [Pos], [Pos]) -- (hm, nextToVisit, visited, count)


nextToVisit :: IntermediateBasin -> [Pos]
nextToVisit (hm, [], pastVisited, basin) = basin
nextToVisit (hm, (x,y):restOfBoundary, pastVisited, basin) =
    if isInBasin
    then nextToVisit (hm, restOfBoundary ++ adjUnvisitedPoints, (x,y):pastVisited, (x,y):basin)
    else nextToVisit (hm, restOfBoundary, (x,y):pastVisited, basin)
    where
        (maxX, maxY) = getDimensions hm
        isInBasin = (9 >) $ getHeight hm (x, y)
        adjPoints = getAdjPos (x,y)
        validAdjPoints = filter (isValidPos maxX maxY) adjPoints
        adjUnvisitedPoints = filter (`notElem` (restOfBoundary ++ pastVisited)) validAdjPoints

getBasins :: HeatMap -> [[Pos]]
getBasins hm = map nextToVisit initIntermediateBasins
    where
        lowPoints = getLowPoints hm
        initIntermediateBasins = map (\p -> (hm, [p], [], [])) lowPoints


-- getBasin :: HeatMap -> Pos -> [Pos]

