{-# LANGUAGE OverloadedStrings #-}

module TransformersSpec where

import Transformers
import Test.Hspec
import Control.Monad.Reader
import Control.Monad.Writer

spec :: Spec
spec = do
    describe "a ReaderT Writer transformer onion" $ do
        it "first adds Reader then Writer" $ do
            runWriter (runReaderT readerWriterExample 3) `shouldBe` (4, "3")