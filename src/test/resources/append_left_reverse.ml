append_left_reverse t1 t2 = match t1 with
  | nil       -> t2
  | (l, x, r) -> append_left_reverse l (t2, x, nil)

(**
 * The number of recursive calls is equivalent to
 * the "leftmost depth" of t1. We interpret t1 as
 * a list, where all elements are the left child
 * nodes, starting from t1.
 * We discard r.
 * The size of t2 is irrelevant when determining
 * the number of recursive calls to append_left_reverse!
 * We therefore expect cost to be expressed only
 * in some terms dependent on t1.
 * We think that our type system cannot solve this.
 *)
