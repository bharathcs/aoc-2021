import Data.List (genericLength, sort)
import Data.Ratio ((%))
main :: IO ()
main = do
    let sample = parse "16,1,2,0,4,2,7,1,2,14\n"
    print sample
    print $ analyseP1 sample
    print $ analyseP2 sample

    fileContents <- readFile "input.txt"
    print $ analyseP1 $ parse fileContents
    print $ analyseP2 $ parse fileContents

-- Utils

type Pos = Int

parse :: String -> [Pos]
parse = map read . splitAtComma . head . lines

splitAtComma :: String -> [String]
splitAtComma s = case dropWhile (== ',') s of
  "" -> []
  s' -> w : splitAtComma s''
    where (w, s'') = break (== ',') s'

-- Part One

analyseP1 :: [Pos] -> Int
analyseP1 xs = sum [abs (x - median) | x <- xs]
    where median = sort xs !! (length xs `div` 2)


analyseP2 xs = min (f (floor mean)) (f (ceiling mean))
    where mean = sum xs % length xs
          f mean = sum [diff * (diff + 1) `div` 2 | x <- xs, let diff = abs (x - mean)]