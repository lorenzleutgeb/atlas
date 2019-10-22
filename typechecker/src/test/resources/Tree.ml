singleton x = (nil, x, nil)

id t = match t with | (a, b, c) -> (a, b, c)

left t = match t with | (l, x, r) -> l

right t = match t with | (l, x, r) -> r

flip t = match t with | (l, x, r) -> (r, x, l)

empty t = match t with | nil -> true | (r, x, l) -> false

clone x t = (t, x, t)

(**
 * Attempt to annotate:
 *   contains_unordered x t | 1 * rk(t)
 *
 * Attempt to prove:
 * Case: t == nil
 *   rk(t)   >= 0
 *   rk(nil) >= 0
 *   0       >= 0
 * Case: t == (l, y, r)
 *   Case: x != y
 *     Case: contains_unordered x l == true
 *       rk(t)                                 >= rk(l) + 1
 *       rk((l, y, r))                         >= rk(l) + 1
 *       rk(l) + log'(|l|) + log'(|r|) + rk(r) >= rk(l) + 1
 *               log'(|l|) + log'(|r|) + rk(r) >=         1
 * !     Error, since for l == nil and r == nil we have that 0 >= 1.
 *     Case: contains_unordered x l == false is symmetric.
 *   Case: x == y
 *     rk(t) >= 0
 *
 * -------------------------------------------------------------------
 *
 * Attempt to annotate:
 *   contains_unordered x t | 1 * p_{(1, 2)}
 *   ...                    | 1 * log'(1 * |t| + 2)
 *   ...                    |     log'(    |t| + 2)
 *
 * Attempt to prove:
 * Case: t == nil
 *   log'(|t| + 2) >= 0
 *   log'(|t| + 2) >= log'(2) = 1 >= 0
 * Case: t == (l, y, r)
 *   Case: x != y
 *     Case: contains_unordered x l == true
 *       log'(|t|         + 2) >= log'(|l| + 2) + 1
 *       log'(|(l, y, r)| + 2) >= log'(|l| + 2) + 1
 *       log'(|l| + |r|   + 2) >= log'(|l| + 2) + 1
 * !     Error, since for l == nil and r == nil we have that 1 >= 2.
 *     Case: contains_unordered x l == false is symmetric.
 *   Case: x == y
 *     log'(|t| + 2) >= 0
 *
 * -------------------------------------------------------------------
 *
 * Attempt to annotate with new potential `ht` (short for "height"):
 *   ht(nil)      := 1
 *   ht((l, _, r) := max({ht(l), ht(r)}) + 1
 *
 *   contains_unordered x t | ht(t)
 *
 * Attempt to prove:
 * Case: t == nil
 *   ht(t) >= 0
 *   by definition of ht.
 * Case: t == (l, y, r)
 *   Case: x != y
 *     Case: contains_unordered x l == true
 *       ht(t) >= ht(l) + 1
 *       ht((l, y, r)) >= ht(l) + 1
 *       max({ht(l), ht(r)}) + 1 >= ht(l) + 1
 *       max({ht(l), ht(r)})     >= ht(l)
 *       Error, since for ht(r) > ht(l)
 *       Case: ht(l) >= ht(r)
 *         ht(l) >= ht(l)
 *       Case: ht(l) < ht(r)
 *         ht(r) >= ht(l)
 *   Case: x == y
 *     ht(t) >= 0
 *     by definition of ht.
 *)
contains_unordered x t = match t with
    | nil       -> false
    | (l, y, r) -> if x == y
        then true
        else (Bool.or (contains_unordered x l) (contains_unordered x r))

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
iter t = match t with
  | nil       -> nil
  | (l, x, r) -> (iter l, x, iter r)
