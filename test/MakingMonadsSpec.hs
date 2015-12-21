module MakingMonadsSpec where

import Test.Hspec
import MakingMonads

spec :: Spec
spec = do
    describe "The State Monad" $ do
        it "given an empty stack when pushing 3 then the stack contains 3" $ do
            runState (push 3) [] `shouldBe` ((), [3])
        it "given an empty stack when pushing 3 and pop then the result is 3 and the stack is empty" $ do
            runState push3ThenPop [] `shouldBe` (3, [])

type Stack = [Int]

pop :: State Stack Int  
pop = State $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs)

push3ThenPop :: State Stack Int
push3ThenPop = do
    push 3
    pop