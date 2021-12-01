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
analysePartTwo = countIncreasing . combineThree . parse

-- helper functions

parse :: String -> [Integer]
parse input = map (read :: String -> Integer) (lines input)

countIncreasing :: [Integer] -> Integer 
countIncreasing (x:y:xs) = (if y > x then 1 else 0) + countIncreasing(y:xs)
countIncreasing [x] = 0
countIncreasing [] = 0

combineThree :: [Integer] -> [Integer]
combineThree (x:y:z:xs) = x+y+z: combineThree (y:z:xs)
combineThree [x, y] = []
combineThree [x] = []
combineThree [] = []