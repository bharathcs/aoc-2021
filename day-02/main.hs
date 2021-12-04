import Data.List

main :: IO()

main = do
  fileContents <- readFile "input.txt"

  print "Part One:"
  let res = analysePartOne fileContents
  print $ "h: " ++ show (h res) ++ " v: " ++ show (v res)
  print $ "product: " ++ show (h res * v res)

  print "Part Two:"
  let res2 = analysePartTwo fileContents
  print $ posToString res2
  print $ "product " ++ show (dist res2 * d res2)


-- Part One

data IntVector = IntVector {h :: Int, v :: Int}

analysePartOne :: String -> IntVector
analysePartOne input = sumVectors (map lineToVector (lines input))

lineToVector :: String -> IntVector
lineToVector = tupleToVector . parseToTuple

parseToTuple :: String -> (String, Int)
parseToTuple = (\x -> (x!!0, read (x!!1))) . words

tupleToVector :: (String, Int) -> IntVector
tupleToVector t = case direction of
  "forward" -> IntVector { h = magnitude, v = 0}
  "up" -> IntVector { h = 0, v = -magnitude }
  "down" -> IntVector { h = 0, v = magnitude }
  _ -> IntVector { h = 999999, v = 999999 }
  where direction = fst t
        magnitude = snd t

addVectors :: IntVector -> IntVector -> IntVector
addVectors a b = IntVector {h = h a + h b, v = v a + v b}

sumVectors :: [IntVector] -> IntVector
sumVectors = foldl addVectors IntVector {h = 0, v = 0}

-- Part Two

analysePartTwo :: String -> TruePosition
analysePartTwo input = calculate (map lineToTruePosition (lines input))

lineToTruePosition :: String -> TruePosition
lineToTruePosition = (\p -> TruePosition {d = 0, dist = h p, a = v p}) . lineToVector

calculate :: [TruePosition] -> TruePosition
getNextPosition :: TruePosition -> TruePosition -> TruePosition

data TruePosition = TruePosition {d :: Int, dist :: Int, a :: Int}
posToString :: TruePosition -> [Char]
posToString p = "depth: " ++ show (d p) ++ " dist: " ++ show (dist p) ++ " aim: " ++ show (a p)

calculate = foldl getNextPosition zeroPosition
  where zeroPosition = TruePosition {d = 0, dist = 0, a = 0}

getNextPosition p1 p2  = TruePosition { d = newDepth, dist = dist p1 + dist p2, a = a p1 + a p2 }
  where newDepth = d p1 + dist p2 * (a p1 + a p2)
