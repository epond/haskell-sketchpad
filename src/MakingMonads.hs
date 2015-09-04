module MakingMonads where

import Data.Ratio  
  
newtype ProbList a = ProbList { getProbList :: [(a,Rational)] } deriving Show

instance Functor ProbList where  
    fmap f (ProbList xs) = ProbList $ map (\(x,p) -> (f x,p)) xs

flatten :: ProbList (ProbList a) -> ProbList a  
flatten (ProbList xs) = ProbList $ concat $ map multAll xs  
    where multAll (ProbList innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

instance Monad ProbList where  
    return x = ProbList [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = ProbList []