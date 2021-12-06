main :: IO ()
main = do
    -- let sample = parse "3,4,3,1,2\n"
    fileContents <- readFile "input.txt"
    let population = parse fileContents

    let simulate80Days = "After " ++ show 80 ++ " days: " ++ show (length (repeater 80 simulate population)) -- Part 1
    let simulate256Days = "After " ++ show 256 ++ " days: " ++ show (sumPop (repeater 256 optimisedSimulate (getPop population))) -- Part 2
    print simulate80Days
    print simulate256Days

-- utils
type LanternAge = Int

parse :: String -> [LanternAge]
parse input = map read $ splitAtComma (head $ lines input)

splitAtComma :: String -> [String]
splitAtComma s = case dropWhile (== ',') s of
  "" -> []
  s' -> w : splitAtComma s''
    where (w, s'') = break (== ',') s'

repeater :: Int -> (a -> a) -> (a -> a)
repeater n f
    | n < 0 = error "Negative numbers not allowed as input to repeater"
    | n == 0 = id
    | n == 1 = f
    | otherwise = f . repeater (n - 1) f

-- Part One

simulate :: [LanternAge] -> [LanternAge]
simulate (x:xs)
    | x == 0 = 6 : 8 : simulate xs
    | otherwise = x - 1 : simulate xs
simulate [] = []

-- Part Two

type Pop = (Int, Int, Int, Int, Int, Int, Int, Int, Int) -- day 0 to 8

getPop :: [Int] -> Pop
getPop arr = let [a, b, c, d, e, f, g, h, i] = countIntoBuckets 9 arr in (a, b, c, d, e, f, g, h, i)

countIntoBuckets :: Int -> [Int] -> [Int]
countIntoBuckets n xs = map (length . (\x -> filter (==x) xs)) [0..(n-1)]

sumPop :: Pop -> Int 
sumPop (a, b, c, d, e, f, g, h, i) = a + b + c + d + e + f + g + h + i

optimisedSimulate :: Pop -> Pop
optimisedSimulate (a, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h + a, i, a)

