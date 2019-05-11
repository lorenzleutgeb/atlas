(* push :: (T a * a) -> T a *)
push q t = (q, t, nil)

(* pop :: (T a) -> T a *)
pop q    = match q with | (p, _, _) -> p | nil -> nil

(* ILLEGAL: We cannot construct trees of trees, only trees of the basic type.

(* get :: (T (T a)) -> (T a) *)
get  q   = match q with | (p, t, _) -> t | nil -> nil

(* a is the element to look for in all elements of q *)
(* q is a list of trees *)
(* dfs_internal :: (a * (Tree (Tree a))) -> Bool *)
dfs_internal a q = match q with
  | nil -> false
  | (p, t, _) -> match t with
    | nil -> false
    | (l, b, r) -> if b = a
      then true
      else let pl = push p l
        (* ILLEGAL: We cannot use neither boolean variables
         * nor the return value of functions as conditions. *)
        in if dfs_internal a pl
        then true
        else let pr = push p r
          (* ILLEGAL: We cannot use neither boolean variables
           * nor the return value of functions as conditions. *)
          in if dfs_internal a pr
          then true
          else false

(* dfs :: (a * (Tree a)) -> Bool *)
dfs a t = dfs_internal a t (nil, t, nil)

*)
