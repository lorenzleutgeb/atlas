walk_l x = match x with | (y, _, _) -> (walk_l y)
walk_r x = match x with | (_, _, y) -> (walk_r y)
