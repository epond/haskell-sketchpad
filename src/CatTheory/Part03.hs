module CatTheory.Part03 where

newtype MyBool = MyBool { getBool :: Bool }

-- Categories Great and Small
-- http://bartoszmilewski.com/2014/12/05/categories-great-and-small/

instance Monoid MyBool where
    mempty = MyBool True
    MyBool x `mappend` MyBool y = MyBool (x && y)
{-
instance Monoid Bool where
    mempty = False
    mappend = (||)
-}

newtype MyInt = MyInt { getInt :: Int }

instance Monoid MyInt where
    mempty = MyInt 0
    MyInt x `mappend` MyInt y = MyInt $ mod (x + y) 3