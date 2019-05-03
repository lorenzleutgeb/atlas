contains_unordered t a = match t with
    | (x, b, y) -> if b == a
        then true
        (* The use of a call expression as condition is problematic since there are no
         * rules on how to deal with that for resource bound inference. *)
        else if contains_unordered x a
            then true
            else contains_unordered y a
    | nil -> false
