module HaskellProgramming.Cipher where

import Data.Char

-- Caesar Cipher

caesar :: Int -> [Char] -> [Char]
caesar n = map (rot n)

unCaesar :: Int -> [Char] -> [Char]
unCaesar n = map (rot (-n))

rot :: Int -> Char -> Char
rot n c = chr . (+97) . (flip mod 26) . (+n) . (flip (-) 97) . ord $ c

-- Vigenère Cipher

vigenere :: [Char] -> [Char] -> [Char]
vigenere _ [] = []
vigenere [] input = input
vigenere keyword input = zipWith rotByChar rotmap input
    where rotmap = take (length input) . concat $ repeat keyword

unVigenere :: [Char] -> [Char] -> [Char]
unVigenere _ _ = ""

rotByChar :: Char -> Char -> Char
rotByChar c = rot . (flip (-) 97) $ ord . toLower $ c