islist_left t = match t with
  | nil       -> true
  | (l, x, r) -> if r == nil
    then islist_left l
    else false
