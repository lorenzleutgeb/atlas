postorder t q = match t with
  | nil       -> q
  | (l, x, r) -> (postorder l (postorder r (q, x, nil)))
