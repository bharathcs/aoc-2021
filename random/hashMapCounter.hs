import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)

-- Making a map with key k (which inherits Ord) and value type Int
-- use store map to take in a list of k and output a map that 
-- on lookup gives k -> Maybe (num of counts of k inserted)
-- Use lookupElse0 to get 0 when k doesn't exist.

storeMap :: Ord k => [k] -> Map k Int
storeMap = foldl insertIntoMap Map.empty
lookupElse0 :: Ord k => k -> Map k Int -> Int
lookupElse0 p currMap = fromMaybe 0 (Map.lookup p currMap)
insertIntoMap :: Ord k => Map k Int -> k -> Map k Int
insertIntoMap currMap p = Map.insert p (lookupElse0 p currMap + 1) currMap