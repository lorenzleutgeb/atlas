pop_right t = match t with
  | nil       -> nil
  | (l, x, r) -> r
