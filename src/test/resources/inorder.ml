inorder t1 t2 = match t1 with
  | nil       -> t2
  | (l, x, r) -> (inorder l (inorder r t2, x, nil))
