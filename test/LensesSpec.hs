{-# LANGUAGE TemplateHaskell #-}

module LensesSpec where

import Control.Lens
import Test.Hspec

type Degrees = Double
type Latitude = Degrees
type Longitude = Degrees

data Meetup = Meetup { _name :: String, _location :: (Latitude, Longitude) }
makeLenses ''Meetup

meetupLat :: Lens' Meetup Latitude
meetupLat = location . _1
meetupLon :: Lens' Meetup Longitude
meetupLon = location . _2

spec :: Spec
spec = do
    describe "the built in tuple lens" $ do
        it "can get the first part of a tuple" $ do
            view _1 ("aa", "bb") `shouldBe` "aa"
        it "can get the second part of a tuple" $ do
            view _2 ("aa", "bb") `shouldBe` "bb"
        it "can modify the first part of a tuple" $ do
            over _1 (++ "!!!") ("aa", "bb") `shouldBe` ("aa!!!", "bb")
        it "can set the first part of a tuple" $ do
            set _1 "cc" ("aa", "bb") `shouldBe` ("cc", "bb")
    describe "generated lenses on a custom type" $ do
        it "exist for each field that starts with an underscore" $ do
            (view name $ Meetup "Haskell User Group" (51.5, 0.1)) `shouldBe` "Haskell User Group"
        it "can be combined" $ do
            (view (location . _1) $ Meetup "Haskell User Group" (51.5, 0.1)) `shouldBe` 51.5
        it "can be combined and extracted to a named function" $ do
            (view meetupLon $ Meetup "Haskell User Group" (51.5, 0.1)) `shouldBe` 0.1
