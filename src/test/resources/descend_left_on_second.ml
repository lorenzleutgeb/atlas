descend_left_on_second t1 t2 = match t2 with
  | nil       -> nil
  | (l, x, r) -> descend_left_on_second t1 l

(**
 * This function is equivalent to
 *
 *     f t1 t2 = nil
 *
 * on trees, but costs the "leftmost depth"
 * of t2.
 *)
