import Data.List (sort)
main = do
    let sample = parse "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]\n"
    fileContents <- readFile "input.txt"
    -- print $ show $ sum $ map (getValue . analyse) sample
    print $ show $ sum $ map (getValue . analyse) $ parse fileContents
    print $ analyseP2 (0, 0, 0, 0, [Square ,Round, Curly, Angle], True)
    print $ analyseP2 (0, 0, 0, 0, [Square ,Round], True)
    let results = filter (> 0) $ map (analyseP2 . analyse) $ parse fileContents
    let midpt = length results `div` 2 -- zero-indexed midpt
    print (midpt, length results)
    print (sort results)
    print $ sort results !! midpt


-- utils

type Line = [Char]
data Token = Round | Square | Curly | Angle | None deriving Eq
data TokenType = Open | Close
type State = (Int, Int, Int, Int, [Token], Bool) -- count of ( [ { <, last char, isValid

instance Show Token where
    show Round = "Round"
    show Square = "Square"
    show Curly = "Curly"
    show Angle = "Angle"
    show None = "None"

parse :: String -> [Line]
parse = lines

parseLine :: Line -> [(TokenType, Token)]
parseLine = map tokenise

tokenise :: Char -> (TokenType, Token)
tokenise '(' = (Open, Round)
tokenise '[' = (Open, Square)
tokenise '{' = (Open, Curly)
tokenise '<' = (Open, Angle)
tokenise ')' = (Close, Round)
tokenise ']' = (Close, Square)
tokenise '}' = (Close, Curly)
tokenise '>' = (Close, Angle)
tokenise _ = error "Not a valid token."


-- part one

analyse :: Line -> State -- isValid, state
analyse x = foldl (\prevState (tt, t) -> nextState prevState tt t) (0, 0, 0, 0, [], True) tokens
    where tokens = parseLine x


nextState :: State -> TokenType -> Token -> State
nextState (_, _, _, _, [], True) Close b = (0,0,0,0, [b], False)
nextState (w, x, y, z, ps, True) Open b = case b of
    Round -> (w+1, x, y, z, b:ps, True)
    Square -> (w, x+1, y, z, b:ps, True)
    Curly -> (w, x, y+1, z, b:ps, True)
    Angle -> (w, x, y, z+1, b:ps, True)
    None -> error "Reached None at this stage."
nextState (w, x, y, z, prev:rest, True) Close b
    | (prev /= None) && (prev /= b) = (w,x,y,z, b:prev:rest, False)
    | otherwise = case b of
        Round -> if w >= 1 then (w-1, x, y, z, rest, True) else (w, x, y, z, b:prev:rest, False)
        Square -> if x >= 1 then (w, x-1, y, z, rest, True) else (w, x, y, z, b:prev:rest, False)
        Curly -> if y >= 1 then (w, x, y-1, z, rest, True) else (w, x, y, z, b:prev:rest, False)
        Angle -> if z >= 1 then (w, x, y, z-1, rest, True) else (w, x, y, z, b:prev:rest, False)
        None -> error "Reached None at this stage."
nextState (a, b, c, d, e, False) _ _ = (a, b, c, d, e, False)

getValue :: State -> Int
getValue (_, _, _, _, _, True) = 0
getValue (_, _, _, _, prev:rest, False) = case prev of
  Round -> 3
  Square -> 57
  Curly -> 1197
  Angle -> 25137
  None -> error "Encountered None at getValue"
getValue (_, _, _, _, [], False) = error "Should not reached empty list at getValue"

completeValue :: [Token] -> Int
completeValue [] = 0
completeValue (x:xs) = case x of
  Round -> 5 * completeValue xs + 1
  Square -> 5 * completeValue xs + 2
  Curly -> 5 * completeValue xs + 3
  Angle -> 5 * completeValue xs + 4
  None -> error "Encountered None at complete Value"

analyseP2 :: State -> Int
analyseP2 (_, _, _, _, _, False) = 0
analyseP2 (w, x, y, z, tokens, True) = completeValue $ reverse tokens

--- (2,3,3,0,[Curly,Curly,Square,Square,Round,Curly,Round,Square],True)
--- (2,1,2,1,[Round,Curly,Angle,Square,Curly,Round],True)
--- (2,2,1,1,[Square,Angle,Round,Square,Round,Curly],False)
--- (4,0,3,2,[Curly,Curly,Angle,Curly,Angle,Round,Round,Round,Round],True)
--- (0,3,0,1,[Square,Angle,Square,Square],False)
--- (1,2,2,0,[Round,Curly,Square,Curly,Square],False)
--- (0,4,4,1,[Square,Square,Curly,Curly,Square,Curly,Square,Curly,Angle],True)
--- (3,1,0,4,[Angle,Round,Angle,Round,Angle,Round,Angle,Square],False)
--- (2,2,1,1,[Square,Round,Square,Round,Curly,Angle],False)
--- (1,1,1,1,[Square,Round,Curly,Angle],True)

--- (2,2,1,1,[Square,Angle,Round,Square,Round,Curly],False)
--- (0,3,0,1,[Square,Angle,Square,Square],False)
--- (1,2,2,0,[Round,Curly,Square,Curly,Square],False)
--- (3,1,0,4,[Angle,Round,Angle,Round,Angle,Round,Angle,Square],False)
--- (2,2,1,1,[Square,Round,Square,Round,Curly,Angle],False)