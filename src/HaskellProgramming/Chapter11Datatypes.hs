module HaskellProgramming.Chapter11Datatypes where

import Data.Char (toUpper)
import Data.List (intercalate)

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a  = Node (insert' b left) a right
    | b > a  = Node left a (insert' b right)
insert' _ t = t

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left a right) = f a l
    where l = foldTree f r left
          r = foldTree f z right

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) l = elem x l && isSubsequenceOf xs l

capitaliseWords :: String -> [(String, String)]
capitaliseWords s = map tupelify (words s)
    where tupelify w@(x:xs) = (w, (toUpper x) : xs)
          tupelify _ = ("", "")

capitaliseWord :: String -> String
capitaliseWord [] = []
capitaliseWord (x:xs) = (toUpper x) : xs

isStopWord :: String -> Bool
isStopWord [] = False
isStopWord w = (head . reverse) w == '.'

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

dropWhileExclusive :: (a -> Bool) -> [a] -> [a]
dropWhileExclusive _ [] = []
dropWhileExclusive _ (_:[]) = []
dropWhileExclusive p (_:x':xs) = if p x' then dropWhileExclusive p xs else xs

sentences :: String -> [String]
sentences [] = []
sentences p = (capitaliseWord . glue) (takeWhileInclusive (not . isStopWord) (words p)) : sentences (glue (dropWhileExclusive (not . isStopWord) (words p)))
    where glue = intercalate " "

capitaliseParagraph :: String -> String
capitaliseParagraph p = intercalate " " $ sentences p