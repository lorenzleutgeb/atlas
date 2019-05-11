(* order :: ((Tree a) * (Tree a)) -> (Tree a) *)

inorder t q = match t with
  | nil       ->                       q
  | (l, x, r) -> (inorder l (inorder r q, x, nil))

preorder t q = match t with
  | nil       ->                         q
  | (l, x, r) -> (preorder l (preorder r q), x, nil)


postorder t q = match t with
  | nil       ->                           q
  | (l, x, r) -> (postorder r (postorder l q), x, nil)

