descend t = match t with
  | nil -> nil
  | (l, m, r) -> descend l