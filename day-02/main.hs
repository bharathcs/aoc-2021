import Data.List

main :: IO()

main = do
  fileContents <- readFile "input.txt"
  print "Part One:"
  let res = analysePartOne fileContents
  let output = "h: " ++ show (h res) ++ " v: " ++ show (v res)
  print output
  print $ "product: " ++ show (h res * v res)

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