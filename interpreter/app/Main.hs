module Main where

import InterpreterPrelude

import qualified LeftList
import qualified RightList
import qualified SplayTree
import qualified Bool
import qualified Tree
import qualified PairingHeap
import qualified SkewHeap
import qualified Scratch

show_inorder :: Show a => Tree a -> String
show_inorder Leaf = " _ "
show_inorder (Node l x r) = (show_inorder l) ++ (show x) ++ (show_inorder r)

check :: Eq a => a -> a -> String
check a b = show ((==) a b)

-- Constants

example_t1 = Node (bottom 2) 1 (bottom 3)
example_t2 = Node (bottom 5) 4 (bottom 6)
example_t3 = Node (Node (bottom 4) 2 (bottom 5)) 1 (bottom 3)

to5 = [1, 2, 3, 4, 5] :: [Integer]
to5r = make_list_right to5
to5l = make_list_left to5

main :: IO ()
main = mapM_ putStrLn [
  show_inorder example_t1,
  show_inorder example_t2,
  show_inorder (RightList.append example_t1 example_t2),
  show_inorder (LeftList.append example_t1 example_t2),
  show_inorder (make_list_left to5),
  show_inorder (make_list_right to5),
  (show . read_left) (make_list_left to5),
  (show . read_right) (make_list_right to5),
  (show . read_left) (make_list_right to5),
  (show . read_right) (make_list_left to5),
  show_inorder (RightList.rev_append example_t1 example_t2),
  (show . read_right) (RightList.append to5r to5r),
  (show . read_right) (RightList.rev_append to5r to5r),
  "1 2 3 4 5 1 2 3 4 5:",
  (show . read_left) (LeftList.append to5l to5l),
  show (reverse to5),
  check ((reverse to5) ++ to5)
  (read_left (LeftList.rev_append to5l to5l)),
  check [1, 2, 3] (read_left (LeftList.preorder example_t1 Leaf)),
  check [1, 2, 3, 4, 5] (read_left (LeftList.preorder example_t1 example_t2)),
  check [1, 2, 4, 5, 3] (read_left (LeftList.preorder example_t3 Leaf)),
  check [4, 5, 2, 3, 1] (read_left (LeftList.postorder example_t3 Leaf)),
  check [4, 2, 5, 1, 3] (read_left (LeftList.inorder example_t3 Leaf)),
  check Leaf (LeftList.tl (LeftList.cons 1 Leaf)),
  show (rank example_t3),
  show (rank to5l),
  show (rank to5r)]
