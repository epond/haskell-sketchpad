module HaskellProgramming.Chapter12Adversity where

import Data.List (intercalate)

-- String processing

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe msg = Just msg

replaceThe :: String -> String
replaceThe = (intercalate " ") . (map swap) . words
    where swap msg = case (notThe msg) of
            Just x -> x
            Nothing -> "a"

vowels :: [Char]
vowels = "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = (go 0) . words
    where go acc (x:y:xs) = case (notThe x) of
            Nothing | (startsWithVowel y) -> go (acc + 1) xs
            _ -> go acc (y:xs)
          go acc _ = acc

startsWithVowel :: String -> Bool
startsWithVowel "" = False
startsWithVowel (x:_) = elem x vowels

countVowels :: String -> Integer
countVowels s = toInteger . length $ filter isVowel s
    where isVowel x = elem x vowels

-- Validate the word

newtype Word' =
    Word' String
    deriving (Eq, Show)

countConsonants :: String -> Integer
countConsonants w = (toInteger . length) w - countVowels w

mkWord :: String -> Maybe Word'
mkWord w
    | countVowels w > countConsonants w = Nothing
    | otherwise                         = Just (Word' w)

-- It's only Natural

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i < 0     = Nothing
    | otherwise = Just (go i)
    where go 0 = Zero
          go x = Succ (go (x - 1))