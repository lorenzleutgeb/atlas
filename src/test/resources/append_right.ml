append_right t1 t2 = match t1 with
  | nil       -> t2
  | (l, x, r) -> (l, x, append_right r t2)

(**
 * The number of recursive calls is equivalent to
 * the "rightmost depth" of t1. We interpret t1 as
 * a list, where all elements are the right child
 * nodes, starting from t1.
 * The size of t2 is irrelevant when determining
 * the number of recursive calls to append_right!
 * We therefore expect cost to be expressed only
 * in some terms dependent on t1.
 *)
