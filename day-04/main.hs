main :: IO ()

main = do

  let sample = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7\n"

  fileContents <- readFile "input.txt"
  print $ length $ lines fileContents
  let (x, y) = parse fileContents

  print $ length y
  print $ showBoard $ y !! 98
  print $ getRightBoard y (0:x)

-- Utils

type DrawnNumbers = [Int]
type Board = [[Int]] -- 5 x 5

parse :: String -> (DrawnNumbers, [Board])
parse fileContents = (drawnNums, parseBoards boards )
  where
    (nums:_:boards) = lines fileContents
    drawnNums = map read (splitAtComma nums)

parseBoards :: [String] -> [Board]
parseBoards (a:b:c:d:e:f:xs) = head (parseBoards firstBoardStr) : parseBoards xs
  where firstBoardStr = [a, b, c, d, e]
parseBoards [a, b, c, d, e] = [map (map read . words) [a, b, c, d, e]]
parseBoards _ = []

showBoard :: [[Int]] -> String
showBoard = foldl (\prev row -> prev ++ "\n" ++ innerFold row) "\n"
  where innerFold = foldl (\a b -> a ++ " " ++ show b) ""

splitAtComma :: String -> [String]
splitAtComma s = case dropWhile (== ',') s of
  "" -> []
  s' -> w : splitAtComma s''
    where (w, s'') = break (== ',') s'

-- Part One

markDone :: Int -> Board -> Board
markDone target = map (map (\x -> if x == target then 0 else x))
checkDone :: Board -> Bool
checkDone board = horizontalWin || verticalWin
  where
    horizontalWin = any (all (== 0)) board
    verticalWin = any (all (== 0) . (\x -> map (!! x) board)) [0..4]
sumBoard :: Board -> Int
sumBoard = foldl (\prev curr -> prev + sum curr) 0

getRightBoard :: [Board] -> DrawnNumbers -> Int
getRightBoard boards nums = if not (null winningBoard) 
  then sumBoard winningBoard * currNum
  else getRightBoard (map (markDone nextNum) boards) (tail nums)
  where
    winningBoards = filter checkDone boards
    winningBoard = if not (null winningBoards) then head winningBoards else []
    currNum = head nums
    nextNum = head $ tail nums

