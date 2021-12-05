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
