splay_max t = match t with
    | nil -> nil
    | (l, b, r) -> match r with
        | nil -> (l, b, nil)
        | (rl, c, rr) -> if rr == nil
            then ((l, b, rl), c, nil)
            else match splay_max rr with
                | (rrl, x, xa) -> (((l, b, rl), c, rrl), x, xa)
