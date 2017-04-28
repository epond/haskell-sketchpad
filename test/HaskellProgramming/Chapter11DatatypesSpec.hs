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