splitAtComma :: String -> [String]
splitAtComma s = case dropWhile (== ',') s of
  "" -> []
  s' -> w : splitAtComma s''
    where (w, s'') = break (== ',') s'
