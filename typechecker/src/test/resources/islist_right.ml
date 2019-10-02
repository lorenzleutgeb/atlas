islist_right t = match t with
  | nil       -> true
  | (l, x, r) -> if l == nil
    then islist_right r
    else false
