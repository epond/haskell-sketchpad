module HaskellProgramming.Chapter10 where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
                (fromGregorian 1911 5 1)
                (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate ((DbDate u) : rest) = u : (filterDbDate rest)
filterDbDate (_ : rest) = filterDbDate rest

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber ((DbNumber n) : rest) = n : (filterDbNumber rest)
filterDbNumber (_ : rest) = filterDbNumber rest

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = foldr max (UTCTime (fromGregorian 1 1 1) (secondsToDiffTime 0)) (filterDbDate db)

sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr (+) 0 (filterDbNumber db)