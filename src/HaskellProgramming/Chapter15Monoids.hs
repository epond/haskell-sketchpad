module HaskellProgramming.Chapter15Monoids where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = mempty

    mappend (Only x) (Only y) = Only (mappend x y)
    mappend (Only x) Nada = Only (x)
    mappend Nada (Only y) = Only (y)
