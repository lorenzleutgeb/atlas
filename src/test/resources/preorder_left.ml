preorder_left t1 t2 = match t1 with
  | nil       -> t2
  | (l, x, r) -> (preorder_left l (preorder_left r t2), x, nil)
