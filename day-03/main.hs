main :: IO ()

main = do
    fileContents <- readFile "input.txt"
    let arr = lines fileContents

    print "Part One:"
    let gamma = getGamma arr
    let epsilon = getEpsilon gamma

    print $ "Gamma: " ++ show (numToStr gamma) ++ " Epsilon: " ++ show (numToStr epsilon)
    print $ "Gamma: " ++ show (numToInt gamma) ++ " Epsilon: " ++ show (numToInt epsilon)
    print $ "Product: " ++ show (numToInt gamma * numToInt epsilon)


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

getGamma :: [String] -> CustomNum
getEpsilon :: CustomNum -> CustomNum

parse :: [Char] -> [Int]
parse line = 1 : map (\x -> read [x]) line

getGamma input = Binary (map (\x -> if (x * 2) >= head summed then 1 else 0) (tail summed))
    where summed = combineVertically (map parse input)

combineVertically :: [[Int]] -> [Int]
combineVertically input = foldl (zipWith (+)) (map (const 0) (input !! 1)) input

getEpsilon num = case num of
  Binary xs -> Binary (map (\x -> if x == 0 then 1 else 0) xs)
  Decimal x -> getEpsilon (Binary (intToBin x))
