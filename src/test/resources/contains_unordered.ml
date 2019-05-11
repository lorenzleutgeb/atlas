contains_unordered a t = match t with
    | (x, b, y) -> if b == a
        then true
        else if contains_unordered a x
            then true
            else contains_unordered a y
    | nil -> false
