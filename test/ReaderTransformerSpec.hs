{-# LANGUAGE OverloadedStrings #-}

module ReaderTransformerSpec where

import ReaderTransformer
import Control.Monad.Reader
import Test.Hspec

spec :: Spec
spec = do
    describe "low level api functions" $ do
        it "can be composed but must pass APIKey each time" $ do
            let address = do
                    user <- getUser "Fred" apiKey
                    getStreetAddress (email user) apiKey
            address `shouldBe` Just "1 Acacia Road"
    describe "reader-wrapped api functions" $ do
        it "can be composed and only need to provide APIKey at the last moment" $ do
            let addressReader = do
                    user <- getUserReaderT "Fred"
                    getStreetAddressReaderT (email user)
            runReaderT addressReader apiKey `shouldBe` Just "1 Acacia Road"