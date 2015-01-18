{-# LANGUAGE OverloadedStrings #-}

module DatatypesSpec where

import Test.Hspec
import Datatypes

spec :: Spec
spec = do
    describe "clientName" $ do
      it "can get the name of a GovOrg" $ do
        clientName (GovOrg "Nasa") `shouldBe` "Nasa"
      it "can get the name of a Company" $ do
        clientName (Company "Pear Inc." 342 (Person "Jack" "Smith" Male) "CEO") `shouldBe` "Pear Inc."
      it "can get the name of an Individual" $ do
        clientName (Individual (Person "Jack" "Smith" Male) False) `shouldBe` "Jack Smith"

main :: IO ()
main = hspec spec