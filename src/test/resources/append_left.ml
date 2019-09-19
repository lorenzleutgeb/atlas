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
 *)
