module Ch2.List (

) where

data List a = Nil | Cons a (List a) deriving Show

empty :: List a
empty = Nil

cons :: a -> List a -> List a
cons = Cons

isEmpty :: List a -> Bool
isEmpty Nil = True
isEmpty _   = False

head' :: List a -> Either String a
head' Nil = Left "The list is empty"
head' (Cons x _) = Right x

tail' :: List a -> Either String (List a)
tail' Nil = Left "The list is empty"
tail' (Cons _ xs) = Right xs

-- We get persistence at the cost of O(n) in copying
(+++) :: List a -> List a -> List a
Nil +++ ys = ys
(Cons x xs) +++ ys = Cons x (xs +++ ys)

update :: List a -> Int -> a -> List a
update (Cons _ xs) 0 y = Cons y xs
update (Cons x xs) n y = Cons x $ update xs (n - 1) y

-- Exercise 2.1
-- O(n) in time and space
suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes xs = xs : suffixes (tail xs)