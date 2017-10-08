module Common where

isLowerAlpha :: Char -> Bool
isLowerAlpha c = c >= 'a' && c <= 'z'


insertAt :: Int -> a -> [a] -> [a]
insertAt 0 y xs     = y : xs
insertAt i _ []     = error "insert failed, out of bounds"
insertAt i y (x:xs) = x : insertAt (i-1) y xs
