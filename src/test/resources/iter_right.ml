iter_right t = match t with
  | nil -> nil
  | (l, x, r) -> (l, x, iter_right r)

(**
 * This function is equivalent to
 *
 *     id x = x
 *
 * on trees, but costs the "rightmost depth"
 * of t.
 *)
