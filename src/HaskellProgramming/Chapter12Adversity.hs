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
