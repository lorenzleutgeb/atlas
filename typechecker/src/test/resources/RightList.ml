cons x t = (nil, x, t)

(**
 * In our setting the tail of nil is nil.
 *)
tl t = match t with
  | nil       -> nil
  | (l, x, r) -> r

(**
 * The number of recursive calls is equivalent to
 * the "rightmost depth" of t1. We interpret t1 as
 * a list, where all elements are the right child
 * nodes, starting from t1.
 * The size of t2 is irrelevant when determining
 * the number of recursive calls to append!
 * We therefore expect cost to be expressed only
 * in some terms dependent on t1.
 *
 * This function is taken from David Obwaller's
 * mail on 2019-09-11.
 *
 * Attempt for annotation is symmetric to append_left.
 *)
append t1 t2 = match t1 with
  | nil       -> t2
  | (l, x, r) -> (cons x (append r t2))

(**
 * This function is equivalent to
 *
 *     f t = nil
 *
 * on trees, but costs the "rightmost depth"
 * of t.
 *)
descend t = match t with
  | nil       -> nil
  | (l, m, r) -> (descend r)

is t = match t with
  | nil         -> true
  | (lx, x, rx) -> match lx with
    | nil         -> is rx
    | (ly, y, ry) -> false

(**
 * This function is equivalent to
 *
 *     id x = x
 *
 * on trees, but costs the "rightmost depth"
 * of t.
 *
 * This function is taken from David Obwaller's
 * mail on 2019-09-11.
 *)
iter t = match t with
  | nil       -> nil
  | (l, x, r) -> (cons x (iter r))

(**
 * The number of recursive calls is equivalent to
 * the "leftmost depth" of t1. We interpret t1 as
 * a list, where all elements are the left child
 * nodes, starting from t1.
 * We discard r.
 * The size of t2 is irrelevant when determining
 * the number of recursive calls to rev_append!
 * We therefore expect cost to be expressed only
 * in some terms dependent on t1.
 * We think that our type system cannot solve this.
 *)
rev_append t1 t2 = match t1 with
  | nil       -> t2
  | (l, x, r) -> (rev_append r (cons x t2))
