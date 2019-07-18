splay a t = match t with
    | nil -> nil
    | (cl, c, cr) -> if a == c
        then (cl, c, cr)
        else if a < c
            then match cl with
                | nil -> (nil, c, cr)
                | (bl, b, br) -> if a == b
                    then (bl, a, (br, c, cr))
                    else if a < b
                        then if bl == nil
                            then (bl, b, (br, c, cr))
                            else match splay a bl with
                                | (al, a', ar) -> (al, a', (ar, b, (br, c, cr)))
                    else if br == nil
                        then (bl, b, (br, c, cr))
                        else match splay a br with
                            | (al, a', ar) ->  ((bl, b, al), a', (ar, c, cr))
            else match cr with
                | nil -> (cl, c, nil)
                | (bl, b, br) -> if a == b
                    then ((cl, c, bl), a, br)
                    else if a < b
                        then if bl == nil
                            then ((cl, c, bl), b, br)
                            else match splay a bl with
                                | (al, a', ar) -> ((cl, c, al), a', (ar, b, br))
                        else if br == nil
                            then ((cl, c, bl), b, br)
                            else match splay a br with
                                | (al, x, xa) -> (((cl, c, bl), b, al), x, xa)
