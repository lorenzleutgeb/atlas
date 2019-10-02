iter_both t = match t with
  | nil       -> nil
  | (l, x, r) -> (iter_both l, x, iter_both r)

(**
 * This function is equivalent to
 *
 *     id x = x
 *
 * on trees, but costs the "depth"
 * of t.
 *
 * This function is taken from David Obwaller's
 * mail on 2019-09-11.
 *
 * -------------------------------------------------------------------
 *
 * Attempt to annotate:
 *   iter_both t | 1 * rk(t)
 *
 * Attempt to prove:
 * Case: t == nil
 *   rk(t)   >= 0
 *   rk(nil) >= 0
 *   0       >= 0
 * Case: t == (l, x, r)
 *   rk(t)                                 >= rk((iter_both l, x, iter_both r)) + 1
 * ! Error, since we cannot expand `(iter_both l, x, iter_both r)` meaningfully.
 *)
