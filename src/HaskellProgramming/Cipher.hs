module HaskellProgramming.Cipher where

import Data.Char

caesar :: Int -> [Char] -> [Char]
caesar n = map (rot n)

unCaesar :: Int -> [Char] -> [Char]
unCaesar n = map (rot (-n))

rot :: Int -> Char -> Char
rot n c = chr . (+97) . (flip mod 26) . (+n) . (flip (-) 97) . ord $ c