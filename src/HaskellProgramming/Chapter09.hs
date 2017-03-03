module HaskellProgramming.Chapter09 where

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
    | x == y    = [x]
    | x < y     = [x, y]
    | otherwise = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
    | x == y    = [x]
    | x < y     = x : eftOrd (succ x) y
    | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt x y
    | x == y    = [x]
    | x < y     = x : eftInt (succ x) y
    | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar x y
    | x == y    = [x]
    | x < y     = x : eftChar (succ x) y
    | otherwise = []

myWords :: String -> [String]
myWords "" = []
myWords (' ' : xs) = myWords xs
myWords s = takeWhile notspace s : myWords (dropWhile notspace s)
    where notspace = (/= ' ')

myLines :: String -> [String]
myLines "" = []
myLines ('\n' : xs) = myLines xs
myLines s = takeWhile notnewline s : myLines (dropWhile notnewline s)
    where notnewline = (/= '\n')
