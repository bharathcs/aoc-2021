import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)
main :: IO ()
main = do
    -- let sample = parse "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf\n" -- actual sample
    let sample = parse "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce\n" -- full list
    fileContents <- readFile "input.txt"

    let answers = map (uncurry getAnswer) $ parse fileContents
    let s = concat answers
    print $ sum [length (filter (== x) s) | x <- [1, 4, 7, 8]]
    print $ [intArrToInt x | x <- answers]
    print $ sum [intArrToInt x | x <- answers]

-- Utils

type TenDigits = [String]
type Output = [String]

parse :: String -> [(TenDigits, Output)]
parse = map (parseLines . words) . lines

parseLines :: [String] -> (TenDigits, Output)
parseLines xs
    | length xs == 15 = (map sort (take 10 xs), map sort (drop 11 xs))
    | otherwise = error "Could not be parsed"

getOfLength :: Int -> TenDigits -> [String]
getOfLength n = filter ((== n) . length)

get1 xs = confirmOneAns "1" $ getOfLength 2 xs
get4 xs = confirmOneAns "4" $ getOfLength 4 xs
get7 xs = confirmOneAns "7" $ getOfLength 3 xs
get8 xs = confirmOneAns "8" $ getOfLength 7 xs

removeLetters :: String -> String -> String
removeLetters lettersToRemove = filter (not . toBeRemoved)
    where toBeRemoved x = x `elem` lettersToRemove

getA known1 known7 = confirmOneAns "A" $ removeLetters known1 known7
getBD known1 known4 = removeLetters known1 known4

get069 :: [String] -> [String]
get069 = filter ((== 6) . length)
get235 :: [String] -> [String]
get235 = filter ((== 5) . length)

get0 :: [String] -> Char -> Char -> String
get0 arr069 bOrD dOrB =  confirmOneAns "0" answer
    where bothBDFilled xs = bOrD `elem` xs && dOrB `elem` xs
          answer = filter (not . bothBDFilled) arr069

getBDinOrder bOrD dOrB known0 = if bOrD `elem` known0 then (bOrD, dOrB) else (dOrB, bOrD)

get6 arr069 known0 known4 known8 = confirmOneAns "6" $ filter is6from6Or9 arr69
    where
        arr69 = filter (/= known0) arr069
        is6from6Or9 sixOrNine = confirmOneAns "missingLetter" (filter (\x -> x `notElem` sixOrNine) known8) `elem` known4

get9 :: [String] -> String -> String -> String
get9 arr069 known0 known6 = confirmOneAns "9" $ filter (/= known6) . filter (/= known0) $ arr069
getC :: String -> String -> Char
getC known6 known8 = confirmOneAns "C" $ filter notIn8 known6
    where notIn8 x = x `notElem` known8
getE :: String -> String -> Char
getE known8 known9 = confirmOneAns "E" $ filter notIn9 known8
    where notIn9 x = x `notElem` known9

get3 :: Char -> Char -> [String] -> String
get3 b e arr235 = confirmOneAns ("3 " ++ show b ++ show e ++ show arr235) $ filter (\x -> b `notElem` x && e `notElem` x) arr235
get5 :: Char -> Char -> [String] -> String
get5 b e arr235 = confirmOneAns "5" $ filter (\x -> b `elem` x) arr235
get2 :: [String] -> String -> String -> String
get2 arr235 known3 known5 = confirmOneAns "2" $ filter (/= known3) . filter (/= known5) $ arr235
confirmOneAns loc answer =
    if length answer == 1
    then head answer
    else error $ "failed to get" ++ loc ++ ". Got " ++ show (length answer) ++ ": " ++ show answer

solve :: TenDigits -> (String -> Int)
solve xs = \x -> fromJust $ x `elemIndex` tenDigits
    where
        known1 = get1 xs
        known7 = get7 xs
        known4 = get4 xs
        known8 = get8 xs
        knownA = getA known1 known7
        bOrD:dOrB:_ = getBD known1 known4
        known0 = get0 arr069 bOrD dOrB
        (knownB, knownD) = getBDinOrder bOrD dOrB known0
        arr069 = get069 xs
        arr235 = get235 xs
        known6 = get6 arr069 known0 known4 known8
        known9 = get9 arr069 known0 known6
        knownC = getC known6 known8
        knownE = getE known8 known9
        known3 = get3 knownB knownE arr235
        known5 = get5 knownB knownE arr235
        known2 = get2 arr235 known3 known5
        tenDigits = [known0, known1, known2, known3, known4, known5, known6, known7, known8, known9] :: [String]

getAnswer :: TenDigits -> Output -> [Int]
getAnswer td = map (solve td)

intArrToInt :: [Int] -> Int
intArrToInt xs = foldl (\ x -> (+) (x * 10)) 0 xs