module ExistentialTypesSpec where

import Test.Hspec
import ExistentialTypes

spec :: Spec
spec = do
    describe "Existential Types" $ do
      it "can be used to express a heterogeneous list where each element satisfies the Show typeclass" $ do
        show (heteroList !! 0) `shouldBe` "()"
        show (heteroList !! 1) `shouldBe` "5"
        show (heteroList !! 2) `shouldBe` "True"

main :: IO ()
main = hspec spec