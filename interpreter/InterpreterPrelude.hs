module InterpreterPrelude
  ( module InterpreterPrelude
  , module Tree
  ) where

import Tree

-- Utility functions to compute potentials

log' :: Integer -> Double
log' n = (logBase 2 (fromInteger n))

leaves :: Tree a -> Integer
leaves Nil = 1
leaves (Node l _ r) = (leaves l) + (leaves r)

rank :: Tree a -> Double
rank Nil = 0
rank (Node l _ r) = (rank l) + (log' (leaves l)) + (rank r) + (log' (leaves r))
