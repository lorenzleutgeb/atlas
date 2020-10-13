module InterpreterPrelude
  ( module InterpreterPrelude
  ) where

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Eq, Show, Functor)

--data Tree a where
--  Leaf :: Tree a
--  Node :: Tree a -> a -> Tree a -> Tree a

bottom :: a -> Tree a
bottom x = Node Leaf x Leaf

make_list_left :: [a] -> Tree a
make_list_left [] = Leaf
make_list_left (x:xs) = Node (make_list_left xs) x Leaf

make_list_right :: [a] -> Tree a
make_list_right [] = Leaf
make_list_right (x:xs) = Node Leaf x (make_list_right xs)

read_left :: Tree a -> [a]
read_left Leaf = []
read_left (Node l x r) = x:(read_left l)

read_right :: Tree a -> [a]
read_right Leaf = []
read_right (Node l x r) = x:(read_right r)

-- Utility functions to compute potentials

log' :: Integer -> Double
log' n = (logBase 2 (fromInteger n))

leaves :: Tree a -> Integer
leaves Leaf = 1
leaves (Node l _ r) = (leaves l) + (leaves r)

rank :: Tree a -> Double
rank Leaf = 0
rank (Node l _ r) = (rank l) + (log' (leaves l)) + (rank r) + (log' (leaves r))
