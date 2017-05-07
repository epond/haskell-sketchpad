module HaskellProgramming.Cipher where

import Data.Char

-- Caesar Cipher

caesar :: Int -> [Char] -> [Char]
caesar = map . rot

unCaesar :: Int -> [Char] -> [Char]
unCaesar n = map (rot (-n))

rot :: Int -> Char -> Char
rot n c = chr . (+97) . (flip mod 26) . (+n) . (flip (-) 97) . ord $ c

-- Vigenère Cipher

vigenere :: [Char] -> [Char] -> [Char]
vigenere = vigenere' rotForwardByChar

unVigenere :: [Char] -> [Char] -> [Char]
unVigenere = vigenere' rotBackwardByChar

vigenere' :: (Char -> Char -> Char) -> [Char] -> [Char] -> [Char]
vigenere' _ _ [] = []
vigenere' _ [] input = input
vigenere' rotByChar keyword input = zipWith rotByChar rotmap input
    where rotmap = take (length input) . concat $ repeat keyword

rotForwardByChar :: Char -> Char -> Char
rotForwardByChar c = rot . (flip (-) 97) $ ord . toLower $ c

rotBackwardByChar :: Char -> Char -> Char
rotBackwardByChar c = rot . ((-) 97) $ ord . toLower $ c