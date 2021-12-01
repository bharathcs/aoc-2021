partOne :: [Integer] -> Integer
partOne (x:xs) = x
partOne [] = 0

analyse :: String -> Integer
analyse = countIncreasing . parse

parse :: String -> [Integer]
parse input = map (read :: String -> Integer) (lines input)

countIncreasing :: [Integer] -> Integer 
countIncreasing (x:y:xss) = (if y > x then 1 else 0) + countIncreasing(y:xss)
countIncreasing [x] = 0
countIncreasing [] = 0

main :: IO()
main = do
  fileContents <- readFile "input1.txt"
  print "Part One:"
  print $ analyse fileContents
