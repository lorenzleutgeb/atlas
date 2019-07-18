preorder t q = match t with
  | nil       -> q
  | (l, x, r) -> (preorder l (preorder r q), x, nil)
