--{-# LANGUAGE GADTs #-}
--data Tree a where
--  Nil :: Tree a
--  Node :: Ord a => Tree a -> a -> Tree a -> Tree a

data Tree a = Nil | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

-- Function definitions are ordered alphabetically.

append_left :: Tree a -> Tree a -> Tree a
append_left Nil t2 = t2
append_left (Node l x r) t2 = Node (append_left l t2) x r

append_right :: Tree a -> Tree a -> Tree a
append_right Nil t2 = t2
append_right (Node l x r) t2 = Node l x (append_right r t2)

append_left_reverse :: Tree a -> Tree a -> Tree a
append_left_reverse Nil t2 = t2
append_left_reverse (Node l x r) t2 = append_left_reverse l (Node t2 x Nil)

append_right_reverse :: Tree a -> Tree a -> Tree a
append_right_reverse Nil t2 = t2
append_right_reverse (Node l x r) t2 = append_right_reverse r (Node Nil x t2)

descend_left :: Tree a -> Tree a
descend_left Nil = Nil
descend_left (Node l x r) = (descend_left l)

descend_right :: Tree a -> Tree a
descend_right Nil = Nil
descend_right (Node l x r) = (descend_right r)

flip :: Tree a -> Tree a
flip Nil = Nil
flip (Node l x r) = Node r x l

inorder_list :: Tree a -> Tree a -> Tree a
inorder_list Nil t2 = t2
inorder_list (Node l x r) t2 = inorder_list l (Node (inorder_list r t2) x Nil)

-- Strange that we need Eq a here, since we are not
-- actually comparing elements but just against Nil.
islist_left :: Eq a => Tree a -> Bool
islist_left Nil = True
islist_left (Node l _ r) = if r == Nil
  then islist_left l
  else False

islist_right :: Eq a => Tree a -> Bool
islist_right Nil = True
islist_right (Node l _ r) = if r == Nil
  then islist_right r
  else False

iter_left :: Tree a -> Tree a
iter_left Nil = Nil
iter_left (Node l x r) = Node (iter_left l) x r

iter_right :: Tree a -> Tree a
iter_right Nil = Nil
iter_right (Node l x r) = Node l x (iter_right r)

preorder_list :: Tree a -> Tree a -> Tree a
preorder_list Nil t2 = t2
preorder_list (Node l x r) t2 = Node (preorder_list l (preorder_list r t2)) x Nil

postorder_list :: Tree a -> Tree a -> Tree a
postorder_list Nil t2 = t2
postorder_list (Node l x r) t2 = postorder_list l (postorder_list r (Node t2 x Nil))

push_left :: Tree a -> a -> Tree a
push_left t x = Node t x Nil

push_right :: Tree a -> a -> Tree a
push_right t x = Node Nil x t

pop_left :: Tree a -> Tree a
pop_left Nil = Nil
pop_left (Node l _ _) = l

pop_right :: Tree a -> Tree a
pop_right Nil = Nil
pop_right (Node _ _ r) = r

-- Utility functions for handling lists

bottom :: a -> Tree a
bottom x = Node Nil x Nil

make_list_left :: [a] -> Tree a
make_list_left [] = Nil
make_list_left (x:xs) = push_left (make_list_left xs) x

make_list_right :: [a] -> Tree a
make_list_right [] = Nil
make_list_right (x:xs) = push_right (make_list_right xs) x

read_left :: Tree a -> [a]
read_left Nil = []
read_left (Node l x r) = x:(read_left l)

read_right :: Tree a -> [a]
read_right Nil = []
read_right (Node l x r) = x:(read_right r)

inorder :: Show a => Tree a -> String
inorder Nil = " _ "
inorder (Node l x r) = (inorder l) ++ (show x) ++ (inorder r)

check :: Eq a => a -> a -> String
check a b = show ((==) a b)

-- Utility functions to compute potentials

log' :: Integer -> Double
log' n = (logBase 2 (fromInteger n))

leaves :: Tree a -> Integer
leaves Nil = 1
leaves (Node l _ r) = (leaves l) + (leaves r)

rank :: Tree a -> Double
rank Nil = 0
rank (Node l _ r) = (rank l) + (log' (leaves l)) + (rank r) + (log' (leaves r))

-- Constants

example_t1 = Node (bottom 2) 1 (bottom 3)
example_t2 = Node (bottom 5) 4 (bottom 6)
example_t3 = Node (Node (bottom 4) 2 (bottom 5)) 1 (bottom 3)

to5 = [1, 2, 3, 4, 5] :: [Integer]
to5r = make_list_right to5
to5l = make_list_left to5

main = mapM_ putStrLn [
  inorder example_t1,
  inorder example_t2,
  inorder (append_right example_t1 example_t2),
  inorder (append_left example_t1 example_t2),
  inorder (make_list_left to5),
  inorder (make_list_right to5),
  (show . read_left) (make_list_left to5),
  (show . read_right) (make_list_right to5),
  (show . read_left) (make_list_right to5),
  (show . read_right) (make_list_left to5),
  inorder (append_right_reverse example_t1 example_t2),
  (show . read_right) (append_right to5r to5r),
  (show . read_right) (append_right_reverse to5r to5r),
  "1 2 3 4 5 1 2 3 4 5:",
  (show . read_left) (append_left to5l to5l),
  show (reverse to5),
  check ((reverse to5) ++ to5)
  (read_left (append_left_reverse to5l to5l)),
  check [1, 2, 3] (read_left (preorder_list example_t1 Nil)),
  check [1, 2, 3, 4, 5] (read_left (preorder_list example_t1 example_t2)),
  check [1, 2, 4, 5, 3] (read_left (preorder_list example_t3 Nil)),
  check [4, 5, 2, 3, 1] (read_left (postorder_list example_t3 Nil)),
  check [4, 2, 5, 1, 3] (read_left (inorder_list example_t3 Nil)),
  check Nil (pop_left (push_left Nil 1)),
  show (rank example_t3),
  show (rank to5l),
  show (rank to5r)]
