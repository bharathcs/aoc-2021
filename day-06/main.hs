main :: IO ()
main = do
    -- let sample = parse "3,4,3,1,2\n"
    fileContents <- readFile "input.txt"
    let population = parse fileContents

    let simulate80Days = repeater 80 simulate
    print $ length $ simulate80Days population

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

