module CatTheory.Part04 where

-- Kleisli Categories
-- http://bartoszmilewski.com/2014/12/23/kleisli-categories/

-- Construct the Kleisli category for partial functions (define composition and identity)

-- Identity for partial functions
return :: a -> Maybe a
return a = Just a

-- Composition for partial functions
(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
m1 >=> m2 = \x ->
    case m1 x of Just y -> m2 y
                 Nothing -> Nothing

-- Implement the embellished function safe_reciprocal that returns a valid reciprocal of
-- its argument, if it’s different from zero
safe_reciprocal :: Double -> Maybe Double
safe_reciprocal 0 = Nothing
safe_reciprocal x = Just $ 1 / x

-- Compose safe_root and safe_reciprocal to implement safe_root_reciprocal that calculates
-- sqrt(1/x) whenever possible
safe_root :: Double -> Maybe Double
safe_root x
    | x >= 0 = Just $ sqrt x
    | otherwise = Nothing

safe_root_reciprocal :: Double -> Maybe Double
safe_root_reciprocal = safe_reciprocal >=> safe_root