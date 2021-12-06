repeater :: Int -> (a -> a) -> (a -> a)
repeater n f
    | n < 0 = error "Negative numbers not allowed as input to repeater"
    | n == 0 = id
    | n == 1 = f
    | otherwise = f . repeater (n - 1) f