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
vigenere _ _ = ""

unVigenere :: [Char] -> [Char] -> [Char]
unVigenere _ _ = ""