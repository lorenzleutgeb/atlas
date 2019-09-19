descend_left t = match t with
  | nil -> nil
  | (l, m, r) -> descend_left l

(**
 * This function is equivalent to
 *
 *     f t = nil
 *
 * on trees, but costs the "leftmost depth"
 * of t.
 *)
