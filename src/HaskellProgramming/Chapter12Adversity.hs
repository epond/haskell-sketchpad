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

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = (go 0) . words
    where go acc (x:y:xs) = case (notThe x) of
            Nothing | (startsWithVowel y) -> go (acc + 1) xs
            _ -> go acc (y:xs)
          go acc _ = acc

startsWithVowel :: String -> Bool
startsWithVowel "" = False
startsWithVowel (x:_) = elem x ['a','e','i','o','u']

countVowels :: String -> Integer
countVowels s = toInteger . length $ filter isVowel s
    where isVowel x = elem x ['a', 'e', 'i', 'o', 'u']