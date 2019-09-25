descend_left t = match t with
  | nil       -> nil
  | (l, x, r) -> descend_left l

(**
 * This function is equivalent to
 *
 *     f t = nil
 *
 * on trees, but costs the "leftmost depth"
 * of t.
 *
 * -------------------------------------------------------------------
 *
 * Attempt to annotate:
 *   descend_left t | 1 * rk(t)
 *
 * Attempt to prove:
 * Case: t == nil
 *   rk(t)   >= 0
 *   rk(nil) >= 0
 *   0       >= 0
 * Case: t == (l, x, r)
 *   rk(t)                                 >= rk(l) + 1
 *   rk((l, x, r))                         >= rk(l) + 1
 *   rk(l) + log'(|l|) + log'(|r|) + rk(r) >= rk(l) + 1
 *           log'(|l|) + log'(|r|) + rk(r) >=         1
 * ! Error, since for l == nil and r == nil we have that 0 >= 1.
 *
 * -------------------------------------------------------------------
 *
 * Attempt to annotate:
 *   descend_left x t | 1 * p_{(1, 2)}
 *   ...              | 1 * log'(1 * |t| + 2)
 *   ...              |     log'(    |t| + 2)
 *
 * Attempt to prove:
 * Case: t == nil
 *   log'(|t| + 2) >= 0
 *   log'(|t| + 2) >= log'(2) = 1 >= 0
 * Case: t == (l, x, r)
 *   log'(|t|         + 2) >= log'(|l| + 2) + 1
 *   log'(|(l, x, r)| + 2) >= log'(|l| + 2) + 1
 *   log'(|l| + |r|   + 2) >= log'(|l| + 2) + 1
 * ! Error, since for l == nil and r == nil we have that 1 >= 2.
 *
 * -------------------------------------------------------------------
 *
 * Attempt to annotate with new potential `ht` (short for "height"):
 *   ht(nil)      := 1
 *   ht((l, _, r) := max({ht(l), ht(r)}) + 1
 *
 *   descend_left x t | ht(t)
 *
 * Attempt to prove:
 * Case: t == nil
 *   ht(t) >= 0
 *   by definition of ht.
 * Case: t == (l, x, r)
 *   ht(t)                   >= ht(l) + 1
 *   ht((l, y, r))           >= ht(l) + 1
 *   max({ht(l), ht(r)}) + 1 >= ht(l) + 1
 *   max({ht(l), ht(r)})     >= ht(l)
 *   Case: ht(l) >= ht(r)
 *     ht(l) >= ht(l)
 *   Case: ht(l) < ht(r)
 *     ht(r) >= ht(l)
 *)
