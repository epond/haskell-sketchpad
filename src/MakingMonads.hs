module MakingMonads where

import Data.Ratio  
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
  
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


-- A State s a is a stateful computation that manipulates a state of type s and has a result of type a.
newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where  
    return x = State $ \s -> (x,s)
    -- Notice a pattern: once we have extracted the result from the monadic value, we apply the function f to it to get the new monadic value.
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState



-- The following is required to suppress the warnings generated from the Applicative - Monad Proposal:

instance Applicative ProbList where
    pure  = return
    (<*>) = ap

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure  = return
    (<*>) = ap
