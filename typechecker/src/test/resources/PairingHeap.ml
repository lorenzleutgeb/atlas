(**
 * The function definitions in this file are taken from or made to match
 * Section 8 of
 *
 *   Tobias Nipkow, Hauke Brinkop
 *   Amortized Complexity Verified
 *   Journal of Automated Reasoning, Vol. 62, Iss. 3, pp. 367-391
 *   https://doi.org/10.1007/s10817-018-9459-3
 *)

(**
 * Original definition:
 *
 *   is_root h = (case h of nil -> true | (l, x, r) -> r == nil)
 *)
is_root h = match h with
  | nil       -> true
  | (l, x, r) -> match r with
    | nil          -> true
    | (lr, xr, rr) -> false

(**
 * Original definition:
 *
 *   pheap nil = true
 *   pheap (l, x, r) = (pheap l /\ pheap r /\ (\forall y \in set_tree l. x <= y))
 *)
pheap h = match h with
  | nil -> true
  | (l, x, r) -> (Bool.and (Bool.and (pheap l) (pheap r)) (all_leq l x))

all_leq t x = match t with
  | nil -> true
  | (l, y, r) -> if y > x
    then false
    else (Bool.and (all_leq l x) (all_leq r x))

(**
 * Original definition:
 *
 *   link nil = nil
 *   link (lx, x, nil) = (lx, x, nil)
 *   link (lx, x, (ly, y, ry)) = (if x < y then ((ly, y, lx), x, ry) else ((lx, x, ly), y, ry))
 *)
link h = match h with
  | nil -> nil
  | (lx, x, rx) -> match rx with
    | nil -> (lx, x, nil)
    | (ly, y, ry) -> if x < y
      then ((ly, y, lx), x, ry)
      else ((lx, x, ly), y, ry)

(**
 * Original definition:
 *
 *   insert x h = merge (nil, x, nil) h
 *)
insert x h = (merge (Tree.singleton x) h)

(**
 * Original definition:
 *
 *   merge nil h = h
 *   merge h nil = h
 *   merge (lx, x, nil) (ly, y, nil) = link (lx, x, (ly, y, nil))
 *
 * But note that we cannot restrict ourselves to the arguments having
 * a right-subtree equal to nil.
 *
 * Proof of a logarithmic upper bound for merge, in analogy to Lemma 8.4:
 *
 * As potential function we take:
 *
 *   Phi nil       := 0
 *   Phi (l, _, r) := Phi l + Phi r + log' |l| + log' |r|
 *
 * where
 *
 *   |nil| := 1    and    |(l, _, r)| := |l| + |r|
 *
 * Let h1 := (lx, x, nil) and h2 := (ly, y, nil)
 *
 * We want to show
 *
 *   Phi (merge h1 h2) - (Phi h1 + Phi h2) =
 *   Phi (merge h1 h2) -  Phi h1 - Phi h2  <=
 *   log' (|h1| + |h2|)
 *
 * First, observe following equality
 *
 *     Phi (merge h1 h2)
 *   = Phi (link (lx, x, h2))                                       (def. merge)
 *   = Phi ((ly, y, lx), x, nil)                                    (w.l.o.g. by def. link)
 *   = Phi (ly, y, lx) + Phi nil + log' |(ly, y, lx)| + log' |nil|  (def. Phi)
 *   = Phi (ly, y, lx) + 0       + log' |(ly, y, lx)| + 0           (def. Phi, |_|)
 *   = Phi (ly, y, lx) + log' (|ly| + |lx|)                         (def. |_|)
 *   = Phi ly + Phi lx + log' |lx| + log' |ly| + log' (|ly| + |lx|) (def. Phi)
 *
 * Then, it follows that
 *
 *      Phi (merge h1 h2) - Phi h1 - Phi h2
 *    = Phi ly + Phi lx + log' |lx| + log' |ly| + log' (|ly + |lx|) (def. Phi)
 *    - Phi lx - Phi nil - log' |lx| - log' |nil|
 *    - Phi ly - Phi nil - log' |ly| - log' |nil|
 *    = ~                                                           (def. Phi, |_|)
 *    - Phi lx - 0       - log' |lx| - 0
 *    - Phi ly - 0       - log' |ly| - 0
 *    = log' (|ly| + |lx|)
 *   <= log' (|lx| + |ly| + 2)                    (monotone log')
 *    = log' (|lx| + |nil| + |ly| + |nil|)
 *    = log' (|h1| + |h2|)
 *
 * For the more general case:
 *
 * Let h1 := (lx, x, rx) and h2 := (ly, y, ry)
 *
 * We want to show
 *
 *   Phi (merge h1 h2) - (Phi h1 + Phi h2) =
 *   Phi (merge h1 h2) -  Phi h1 - Phi h2  <=
 *   log' (|h1| + |h2|)
 *
 * First, observe following equality
 *
 *     Phi (merge h1 h2)
 *   = Phi (link (lx, x, (ly, y, nil)))    (def. merge)
 *   Case x < y:
 *     = Phi ((ly, y, lx), x, nil)                                    (def. link)
 *     = Phi (ly, y, lx) + Phi nil + log' |(ly, y, lx)| + log' |nil|  (def. Phi)
 *     = Phi (ly, y, lx) + 0       + log' (|ly| + |lx|) + 0           (def. Phi, |_|)
 *     = Phi ly + Phi lx + log' |lx| + log' |ly| + log' (|ly| + |lx|) (def. Phi)
 *   Case x >= y:
 *     = Phi ((lx, x, ly), y, nil)                                    (def. link)
 *     = Phi (lx, x, ly) + Phi nil + log' |(lx, x, ly)| + log' |nil|  (def. Phi)
 *     = Phi (lx, x, ly) + 0       + log' (|lx| + |ly|) + 0           (def. Phi, |_|)
 *     = Phi lx + Phi ly + log' |ly| + log' |lx| + log' (|lx| + |ly|) (def. Phi)
 *   = Phi lx + Phi ly + log' |lx| + log' |ly| + log' (|lx| + |ly|) (comm. +)
 *
 * Then, it follows that
 *
 *      Phi (merge h1 h2) - Phi h1 - Phi h2
 *    = Phi lx + Phi ly + log' |lx| + log' |ly| + log' (|lx + |ly|) (def. Phi)
 *    - Phi lx - Phi rx - log' |lx| - log' |rx|
 *    - Phi ly - Phi ry - log' |ly| - log' |ry|
 *    = log' (|ly| + |lx|) - Phi rx - log' |rx| - Phi ry - log' |ry|
 *   <= log' (|ly| + |lx|)                       (log' |_| >= 0, Phi _ >= 0, def. -)
 *   <= log' (|lx| + |rx| + 1 + |ly| + |ry| + 1) (monotone log'))
 *    = log' (|h1| + |h2|)
 *)
merge h1 h2 = match h1 with
  | nil -> h2
  | (lx, x, rx) -> match h2 with
    | nil         -> (lx, x, rx)
    | (ly, y, ry) -> (link (lx, x, (ly, y, nil)))

del_min h = match h with
  | nil       -> nil
  | (l, x, r) -> (pass2 (pass1 l))

pass1 h = match h with
  | nil -> nil
  | (lx, x, rx) -> match rx with
    | nil -> (lx, x, nil)
    | (ly, y, ry) -> (link (lx, x, (ly, y, pass1 ry)))

pass2 h = match h with
  | nil -> nil
  | (l, x, r) -> (link (l, x, pass2 r))

merge_pairs h = match h with
  | nil -> nil
  | (lx, x, rx) -> match rx with
    | nil         -> (lx, x, nil)
    | (ly, y, ry) -> (link (link (lx, x, (ly, y, merge_pairs ry))))

