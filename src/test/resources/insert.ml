insert a t = if t == nil
    then (nil, a, nil)
    else match splay a t with
        | (l, a', r) ->
            if a == a'
                then (l, a, r)
                else if a < a'
                    then (l, a, (nil, a', r))
                    else ((l, a', nil), a, r)
