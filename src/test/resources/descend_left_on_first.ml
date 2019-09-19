descend_left_on_first t1 t2 = match t with
  | nil -> nil
  | (l, x, r) -> descend_left_on_first l t2

(**
 * This function is equivalent to
 *
 *     f t1 t2 = nil
 *
 * on trees, but costs the "leftmost depth"
 * of t1.
 *)
