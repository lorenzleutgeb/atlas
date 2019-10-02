pop_left t = match t with
  | nil       -> nil
  | (l, x, r) -> l
