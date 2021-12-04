main :: IO ()

main = do
    -- let sample = map parse (lines "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010\n")

    fileContents <- readFile "input.txt"
    let arr = map parse (lines fileContents)

    print "Part One:"
    let gamma = getGamma arr
    let epsilon = getEpsilon gamma

    print $ "Gamma: " ++ numToStr gamma ++ " Epsilon: " ++ numToStr epsilon
    print $ "Gamma: " ++ show (numToInt gamma) ++ " Epsilon: " ++ show (numToInt epsilon)
    print $ "Product: " ++ show (numToInt gamma * numToInt epsilon)

    print "Part Two:"
    let o2ratings = getRating O2 (0, arr)
    let co2ratings = getRating CO2 (0, arr)

    print $ "O2: " ++ numToStr o2ratings ++ " CO2: " ++ numToStr co2ratings
    print $ "O2: " ++ show (numToInt o2ratings) ++ " CO2: " ++ show (numToInt co2ratings)
    print $ "Product: " ++ show (numToInt o2ratings * numToInt co2ratings)

-- Utils

data CustomNum = Binary [Int]
    | Decimal Int

numToInt :: CustomNum -> Int
numToStr :: CustomNum -> String 
numToBin :: CustomNum -> [Int]

numToStr num = case num of 
    (Binary xs) -> concatMap show xs
    (Decimal x) -> show x

numToInt num = case num of 
    (Binary xs) -> binToInt xs
    (Decimal x) -> x

numToBin num = case num of
  Binary xs -> xs
  Decimal x -> intToBin x

binToInt :: [Int] -> Int
binToInt = foldl (\x -> (+) (x * 2)) 0
intToBin :: Int -> [Int]
intToBin 0 = [0]
intToBin 1 = [1]
intToBin x = if even x
    then intToBin (div x 2) ++ [0]
    else intToBin (div (x - 1) 2) ++ [1]

-- Part One

getGamma :: [[Int]] -> CustomNum
getEpsilon :: CustomNum -> CustomNum

parse :: [Char] -> [Int]
parse = map (\ x -> read [x])

getGamma input = Binary (map (\x -> if (x * 2) >= head summed then 1 else 0) (tail summed))
    where summed = combineVertically (map (1:) input)

combineVertically :: [[Int]] -> [Int]
combineVertically input = foldl (zipWith (+)) (map (const 0) (input !! 1)) input

getEpsilon num = case num of
  Binary xs -> Binary (map (\x -> if x == 0 then 1 else 0) xs)
  Decimal x -> getEpsilon (Binary (intToBin x))


-- Part Two

data Metric = O2 | CO2

filterByIndex :: Int -> [[Int]] -> ([[Int]], [[Int]])
getRating :: Metric -> (Int, [[Int]]) -> CustomNum
getNextList :: Metric -> (Int, [[Int]]) -> (Int, [[Int]])

filterByIndex n arrOfArrs = (filter (checkBitN 0) arrOfArrs, filter (checkBitN 1) arrOfArrs)
    where checkBitN = \target arr -> (arr !! n) == target

getNextList m (x, xs) = case m of
    O2 -> (x + 1, snd sortedRes)
    CO2 -> (x + 1, fst sortedRes)
    where
        len = length $ head xs
        res = filterByIndex (x `mod` len) xs
        sortedRes = if length (snd res) >= length (fst res) then res else (snd res, fst res)

getRating m (x, xs)
    | length xs == 1 = Binary $ head xs
    | otherwise = getRating m (getNextList m (x, xs))


