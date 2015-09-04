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
    describe "The State Monad" $ do
        it "given an empty stack when pushing 3 then the stack contains 3" $ do
            runState (push 3) [] `shouldBe` ((), [3])
        it "given an empty stack when pushing 3 and pop then the result is 3 and the stack is empty" $ do
            runState push3ThenPop [] `shouldBe` (3, [])

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


type Stack = [Int]

pop :: State Stack Int  
pop = State $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs)

push3ThenPop :: State Stack Int
push3ThenPop = do
    push 3
    pop