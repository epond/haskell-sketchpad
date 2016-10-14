-- Originates from a video by Matthew Brecknell: https://www.youtube.com/watch?v=52VsgyexS8Q

{-# LANGUAGE ScopedTypeVariables #-}

module HoleDrivenDevelopment where

{-  A technique for using types to drive the construction of functional programs.
    We start with the type signatures of a function but the body is undefined.
    Pull apart the arguments and give them names, eg. compose has f, g and x.
    Bind each argument to a wildcard pattern in the scope of a where clause.
    The ScopedTypeVariables language extension allows the type variables in the function
    signature to be scoped over the function definition when we use forall in the signature.
    Provoke a type error by using a noisy hole which tells us the type expected.
    Use one noisy hole at any given time and silent holes elsewhere so that we don't
    have to think about which hole caused the type error.
    Once the function body has been arrived at, remove the where and forall.
-}

-- A noisy hole that always provokes a type error
data Hole = Hole
-- A silent hole which will always be accepted anywhere
hole = undefined
-- If we need many holes at a time we should use one noisy hole with the rest as silent holes
-- so we can tell which hole caused the type error

-- The forall denotes the scope to which we bind the type variables a, b and c
compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
compose f g x = undefined -- Replace this with a noisy hole to begin hole-driven development
    where
        -- This where clause with dummy bindings allows the compiler to check
        -- type assertions made on the function arguments.
        -- You can use a noisy hole here to learn the type of each argument.
        _ = f :: b -> c
        _ = g :: a -> b
        _ = x :: a

{-  In the next example we can see by the function signature that m is a type constructor
    which is constrained to the Monad typeclass. We know more about m than if it were unconstrained.
    We can make use of the methods of the Monad type-constructor class in our implementation.
-}

apply :: forall m a b. Monad m => m (a -> b) -> m a -> m b
apply mf ma = mf >>= k
    where
        _ = (>>=) :: forall h. m h -> (h -> m b) -> m b
        _ = (mf >>=) :: ((a -> b) -> m b) -> m b
        _ = (ma >>=) :: (a -> m b) -> m b
        _ = mf :: m (a -> b)
        _ = ma :: m a
        k f = ma >>= r
            where
                _ = f :: a -> b
                r x = hole
                    where
                        _ = x :: a


filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' = undefined