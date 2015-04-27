module CatTheory.Part04 where

-- Kleisli Categories
-- http://bartoszmilewski.com/2014/12/23/kleisli-categories/

return :: a -> Maybe a
return a = Just a

(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
m1 >=> m2 = \x ->
    case m1 x of Just y -> m2 y
                 Nothing -> Nothing

safe_reciprocal :: Double -> Maybe Double
safe_reciprocal 0 = Nothing
safe_reciprocal x = Just $ 1 / x

safe_root :: Double -> Maybe Double
safe_root x
    | x >= 0 = Just $ sqrt x
    | otherwise = Nothing

safe_root_reciprocal :: Double -> Maybe Double
safe_root_reciprocal = safe_reciprocal >=> safe_root