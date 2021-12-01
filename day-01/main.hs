main :: IO()
main = do
  fileContents <- readFile "input1.txt"
  print "Part One:"
  print $ analysePartOne fileContents
  print "Part Two:"
  print $ analysePartTwo fileContents

-- core functions

analysePartOne :: String -> Integer
analysePartOne = countIncreasing . parse

analysePartTwo :: String -> Integer
analysePartTwo = countIncreasing . windowMaker 3 . parse

-- helper functions

parse :: String -> [Int]
parse input = map (read :: String -> Int) (lines input)

countIncreasing :: [Int] -> Integer 
countIncreasing (x:y:xs) = (if y > x then 1 else 0) + countIncreasing(y:xs)
countIncreasing [x] = 0
countIncreasing [] = 0

windowMaker :: Int -> [Int] -> [Int]

windowMaker i xs 
  | length xs < i = []
  | otherwise = sumFirstI : windowMaker i restOfXs
    where sumFirstI = sumFirst i xs
          restOfXs = tail xs

splitList :: Int -> [Int] -> ([Int], [Int])
splitList 0 xs = ([], xs)
splitList _ [] = ([], [])
splitList i (x:xs) = (x : fst res, snd res)
  where res = splitList (i-1) xs

sumFirst :: Int -> [Int] -> Int
sumFirst i xs = sum (fst (splitList i xs))

-- Simpler implementation of function created by `windowMaker 3`
combineThree :: [Integer] -> [Integer]
combineThree (x:y:z:xs) = x+y+z: combineThree (y:z:xs)
combineThree [x, y] = []
combineThree [x] = []
combineThree [] = []
