preorder t1 t2 = match t1 with
  | nil       -> t2
  | (l, x, r) -> (preorder l (preorder r t2), x, nil)
