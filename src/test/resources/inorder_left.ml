inorder_left t1 t2 = match t1 with
  | nil       -> t2
  | (l, x, r) -> (inorder_left l (inorder_left r t2, x, nil))
