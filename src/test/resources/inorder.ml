inorder t q = match t with
  | nil       -> q
  | (l, x, r) -> (inorder l (inorder r q, x, nil))
