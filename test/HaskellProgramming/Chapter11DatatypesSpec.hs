module HaskellProgramming.Chapter11DatatypesSpec where

import Test.Hspec
import HaskellProgramming.Chapter11Datatypes

spec :: Spec
spec = do
    describe "A binary tree" $ do
        it "can insert to create a new tree" $ do
            insert' 0 Leaf `shouldBe` Node Leaf 0 Leaf
        it "can insert to a tree of one level" $ do
            insert' 5 (insert' 0 Leaf) `shouldBe` Node Leaf 0 (Node Leaf 5 Leaf)
        it "can insert to a tree of two levels" $ do
            insert' 3 (insert' 5 (insert' 0 Leaf)) `shouldBe` Node Leaf 0 (Node (Node Leaf 3 Leaf) 5 Leaf)
        it "can map a function over each node" $ do
            let testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
            let mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
            mapTree (+1) testTree' `shouldBe` mapExpected
        it "can convert to a preordered list" $ do
            preorder (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) `shouldBe` [2, 1, 3]
        it "can convert to an inordered list" $ do
            inorder (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) `shouldBe` [1, 2, 3]
        it "can convert to a postordered list" $ do
            postorder (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) `shouldBe` [1, 3, 2]
        it "has a function to perform a catamorphism" $ do
            foldTree (+) 0 (Node (Node Leaf 4 Leaf) 7 (Node Leaf 3 Leaf)) `shouldBe` 14
            foldTree (*) 1 (Node (Node Leaf 4 Leaf) 7 (Node Leaf 3 Leaf)) `shouldBe` 84
    describe "As-patterns" $ do
        it "can be used to implement a subsequence function" $ do
            isSubsequenceOf "blah" "blahwoot" `shouldBe` True
            isSubsequenceOf "blah" "wootblah" `shouldBe` True
            isSubsequenceOf "blah" "wboloath" `shouldBe` True
            isSubsequenceOf "blah" "wootbla" `shouldBe` False
            isSubsequenceOf "" "blah" `shouldBe` True
            isSubsequenceOf "b" "" `shouldBe` False
        it "can be used to implement a function that capitalises words in a tuple" $ do
            capitaliseWords "hello world" `shouldBe` [("hello", "Hello"), ("world", "World")]
    describe "Language exercises" $ do
        it "can capitalise a word" $ do
            capitaliseWord "Hello" `shouldBe` "Hello"
            capitaliseWord "hello" `shouldBe` "Hello"
        it "can capitalise two sentences in a paragraph" $ do
            capitaliseParagraph "foo blah. woot ha." `shouldBe` "Foo blah. Woot ha."
        it "can capitalise three sentences in a paragraph" $ do
            capitaliseParagraph "foo blah. woot ha. nice." `shouldBe` "Foo blah. Woot ha. Nice."