delete a t = if t == nil
    then nil
    else match splay a t with
        | (l, a', r) -> if a == a'
            then if l == nil
                then r
                else match splay_max l with
                    | (l', m, r') -> (l', m, r)
            else (l, a', r)
