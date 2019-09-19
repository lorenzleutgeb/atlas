contains_unordered x t = match t with
    | (l, y, r) -> if x == y
        then true
        else if contains_unordered x l
            then true
            else contains_unordered x r
    | nil -> false
