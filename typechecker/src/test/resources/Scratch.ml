id x = x

left x y = x

right x y = y

(**
 * As of 2019-11-07 we get the following result, which looks okay:
 *
 *   empty_1 t | log(|t| + 2) + log(2 · |t| + 2) + 1 -> 0
 *)
empty_1 t = (Tree.empty t)

empty_2 t = (empty_1 t)

(**
 * As of 2019-11-07 we get the following result, which looks wrong:
 *
 *   empty_3 t | 0 -> 0
 *
 * By annotating the argument with zero, we cannot pay for the
 * call to Tree.empty! An annotation like for empty_1 would be
 * expected. This suggests that there's a bug in the
 * implementation of annotation generation for (let).
 *)
empty_3 t = (let s = t in (Tree.empty s))

same_root t1 t2 = match t1 with
  | nil -> (if t2 == nil then true else false)
  | (lx, x, rx) -> match t2 with
    | nil -> false
    | (ly, y, ry) -> (if y == x then true else false)

empty_4 t1 t2 = (Bool.or (Tree.empty t1) (Tree.empty t2))

(**
 * As of 2019-11-07 we get the following result, which looks okay:
 *
 *   same_root_obviously t | log(|t| + 2) + log(2 · |t| + 2) + 1 -> 0
 *)
same_root_obviously t = (same_root t t)

(**
 * As of 2019-11-07 we get the following result, which is okay,
 * but could be tighter. Why do we have t2 floating around?
 *
 *   first_nonempty_and_second_empty t1 t2 | log(|t2| + 2) + log(2 · |t2| + 2) + 1 -> 0
 *)
first_nonempty_and_second_empty t1 t2 = match t1 with | (l, x, r) -> Tree.empty t2 | nil -> false
