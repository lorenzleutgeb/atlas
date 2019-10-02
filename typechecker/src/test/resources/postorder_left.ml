postorder_left t1 t2 = match t1 with
  | nil       -> t2
  | (l, x, r) -> (postorder_left l (postorder_left r (t2, x, nil)))
