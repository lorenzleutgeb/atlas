(* order :: (T * T) -> T *)

inorder t q = match t with
  | nil       -> q
  | (l, x, r) -> (inorder l (inorder r q, x, nil))

preorder t q = match t with
  | nil       -> q
  | (l, x, r) -> (preorder l (preorder r q), x, nil)

postorder t q = match t with
  | nil       -> q
  | (l, x, r) -> (postorder l (postorder r (q, x, nil)))

