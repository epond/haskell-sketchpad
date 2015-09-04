module MakingMonads where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
  
-- A State s a is a stateful computation that manipulates a state of type s and has a result of type a.
newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where  
    return x = State $ \s -> (x,s)
    -- Notice a pattern: once we have extracted the result from the monadic value, we apply the function f to it to get the new monadic value.
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState


-- The following is required to suppress the warnings generated from the Applicative - Monad Proposal:

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure  = return
    (<*>) = ap
