all_leq t x = match t with
  | nil -> true
  | (l, y, r) -> if y > x
    then false
    else (Bool.and (all_leq l x) (all_leq r x))

del_min h = match h with
  | nil       -> nil
  | (l, x, r) -> (pass2 (pass1 l))

insert x h = (merge (Tree.singleton x) h)

is_root h = match h with
  | nil       -> true
  | (l, x, r) -> match r with
    | nil          -> true
    | (lr, xr, rr) -> false

link h = match h with
  | nil -> nil
  | (lx, x, rx) -> match rx with
    | nil -> (lx, x, nil)
    | (ly, y, ry) -> if x < y
      then ((ly, y, lx), x, ry)
      else ((lx, x, ly), y, ry)

merge h1 h2 = match h1 with
  | nil -> h2
  | (lx, x, rx) -> match h2 with
    | nil         -> (lx, x, rx)
    | (ly, y, ry) -> (link (lx, x, (ly, y, nil)))

merge_pairs h = match h with
  | nil -> nil
  | (lx, x, rx) -> match rx with
    | nil         -> (lx, x, nil)
    | (ly, y, ry) -> (link (link (lx, x, (ly, y, merge_pairs ry))))

pass1 h = match h with
  | nil -> nil
  | (lx, x, rx) -> match rx with
    | nil -> (lx, x, nil)
    | (ly, y, ry) -> (link (lx, x, (ly, y, pass1 ry)))

pass2 h = match h with
  | nil -> nil
  | (l, x, r) -> (link (l, x, pass2 r))

pheap h = match h with
  | nil -> true
  | (l, x, r) -> (Bool.and (Bool.and (pheap l) (pheap r)) (all_leq l x))
