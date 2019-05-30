walk_l x = match x with | (y, x1, x2) -> (walk_l y)
walk_r x = match x with | (x1, x2, y) -> (walk_r y)
