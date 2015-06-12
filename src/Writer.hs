module Writer where

import Control.Monad.Writer

-- we can use do notation with Writer to multiply two numbers. note how the writer function is used
-- instead of the Writer value constructor to allow for future changes to the Writer implementation.
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])
-- the context here is a log of each number used in the calculation
multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"] -- has dummy value () as its result
    return (a*b)