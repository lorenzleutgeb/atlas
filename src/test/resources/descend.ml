descend t = match t with
(*  | nil -> true *)
  | (l, m, r) -> descend l