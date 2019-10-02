append_left t1 t2 = match t1 with
  | nil       -> t2
  | (l, x, r) -> (append_left l t2, x, r)

(**
 * The number of recursive calls is equivalent to
 * the "leftmost depth" of t1. We interpret t1 as
 * a list, where all elements are the left child
 * nodes, starting from t1.
 * The size of t2 is irrelevant when determining
 * the number of recursive calls to append_left!
 * We therefore expect cost to be expressed only
 * in some terms dependent on t1.
 *
 * -------------------------------------------------------------------
 *
 * Attempt to annotate:
 *   append_left t1 t2 | 1 * rk(t1)
 *
 * Attempt to prove:
 * Case: t1 == (l, x, r)
 *   rk(t1)                                >= rk((append_left l t2, x, r)) + 1
 *   rk((l, x, r))                         >= ...
 *   rk(l) + log'(|l|) + log'(|r|) + rk(r) >= ...
 *   rk(l) + log'(|l|) + log'(|r|) + rk(r) >= rk((append_left l t2, x, r)) + 1
 *   ...                                   >= rk(append_left l t2) + log'(|append_left l t2|) + log'(|r|) + rk(r) + 1
 *   rk(l) + log'(|l|)                     >= rk(append_left l t2) + log'(|append_left l t2|) + 1
 * ! At this point we are stuck, since we do not know how to cancel `append_left l t2`.
 * Case: t1 == nil
 *   rk(t1)  >= 0
 *   rk(nil) >= 0
 *   0       >= 0
 *)
