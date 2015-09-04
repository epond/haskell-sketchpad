module MakingMoadsSpec where

import Test.Hspec
import Data.Ratio
import Data.List (all)
import MakingMonads

spec :: Spec
spec = do
    describe "The ProbList Monad" $ do
        it "calculates the probabilities of three tails in a row with one loaded coin" $ do
            getProbList flipThree `shouldBe` [(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]

data Coin = Heads | Tails deriving (Show, Eq)  
  
coin :: ProbList Coin  
coin = ProbList [(Heads,1%2),(Tails,1%2)]

loadedCoin :: ProbList Coin  
loadedCoin = ProbList [(Heads,1%10),(Tails,9%10)]

flipThree :: ProbList Bool  
flipThree = do  
    a <- coin  
    b <- coin  
    c <- loadedCoin  
    return (all (==Tails) [a,b,c])