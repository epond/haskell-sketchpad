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
myWords = myWay ' '

myLines :: String -> [String]
myLines = myWay '\n'

myWay :: Char -> String -> [String]
myWay _ "" = []
myWay c s
    | c == (head s) = myWay c (tail s)
    | otherwise = takeWhile (/= c) s : myWay c (dropWhile (/= c) s)
