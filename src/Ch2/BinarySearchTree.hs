module Ch2.BinarySearchTree (

) where

import Data.Maybe ( fromMaybe )

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node left y right) 
    | x < y = member x left
    | x > y = member x right
    | otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x t@(Node left y right) 
    | x < y = Node (insert x left) y right
    | x > y = Node left y (insert x right)
    | otherwise = t

-- Exercise 2.2
member' :: Ord a => a -> Tree a -> Bool
member' x = go x where
    go mightEq Leaf = mightEq == x
    go mightEq (Node left y right)
        | x < y = go mightEq left
        | otherwise = go y right

-- Exercise 2.3
insert' :: Ord a => a -> Tree a -> Tree a
insert' x t = fromMaybe t (go t) where
    go Leaf = return (Node Leaf x Leaf)
    go (Node left y right)
        | x < y  =  go left >>= \left' -> 
                    return (Node left' y right)
        | x > y  =  go right >>= \right' ->
                    return (Node left y right') 
        | otherwise = Nothing