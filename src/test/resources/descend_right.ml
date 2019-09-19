descend_right t = match t with
  | nil -> nil
  | (l, m, r) -> descend_right r

(**
 * This function is equivalent to
 *
 *     f t = nil
 *
 * on trees, but costs the "rightmost depth"
 * of t.
 *)
