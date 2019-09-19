postorder t1 t2 = match t1 with
  | nil       -> t2
  | (l, x, r) -> (postorder l (postorder r (t2, x, nil)))
