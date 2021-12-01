main :: IO ()

main = do
  let xs = [1, 2, 3, 4, 5, 6, 7, 8, 9]
  print $ windowMaker 1 xs
  print $ windowMaker 2 xs
  print $ windowMaker 3 xs
  print $ windowMaker 4 xs
  print $ windowMaker 5 xs
  print $ windowMaker 6 xs
  print $ windowMaker 7 xs
  print $ windowMaker 8 xs
  print $ windowMaker 9 xs
  print $ windowMaker 10 xs


splitList :: Int -> [Int] -> ([Int], [Int])
splitList 0 xs = ([], xs)
splitList _ [] = ([], [])
splitList i (x:xs) = (x : fst res, snd res)
  where res = splitList (i-1) xs

sumFirst :: Int -> [Int] -> Int
sumFirst i xs = sum (fst (splitList i xs))

windowMaker :: Int -> [Int] -> [Int]

windowMaker i xs 
  | length xs < i = []
  | otherwise = sumFirstI : windowMaker i restOfXs
    where sumFirstI = sumFirst i xs
          restOfXs = tail xs
