module WriterSpec where

import Test.Hspec
import Control.Monad.Writer
import Writer

spec :: Spec
spec = do
    describe "Adding two numbers together with a writer" $ do
        it "has the expected result value" $ do
            fst (runWriter multWithLog) `shouldBe` 15
        it "has the expected log" $ do
            snd (runWriter multWithLog) `shouldBe` ["Got number: 3","Got number: 5","Gonna multiply these two"]       