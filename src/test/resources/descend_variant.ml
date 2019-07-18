descend_variant t1 t2 = match t1 with
  | nil -> t2
  | (l, m, r) -> descend_variant l (nil, m, t2)