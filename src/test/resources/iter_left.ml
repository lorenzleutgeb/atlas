iter_left t = match t with
  | nil -> nil
  | (l, x, r) -> (iter_left l, x, r)

(**
 * This function is equivalent to
 *
 *     id x = x
 *
 * on trees, but costs the "leftmost depth"
 * of t.
 *)
