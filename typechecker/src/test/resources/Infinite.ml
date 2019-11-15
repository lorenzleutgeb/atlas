(** Contains function definitions that do not terminate. *)

infinite_1 x = (infinite_1 x, x, infinite_1 x)

infinite_2 x t = (infinite_2 x t, x, t)

infinite_3 x t = (infinite_2 x t)

infinite_4 x t = (let s = t in infinite_3 x s)

infinite_5 x t = (let s = t in (infinite_5 x t, x, t))

infinite_6 x t1 t2 = ((infinite_2 x t1), x, t2)

infinite_7 x t1 t2 = (let s = t1 in ((infinite_2 x s), x, t2))

infinite_8 x t = (let s = t in (infinite_8 x s, x, s))

infinite_9 x t1 t2 = (let s = t2 in (infinite_9 x s t1, x, t1))

(**
 * The following is an interesting case, but the current implementation cannot type mutual recursion.
 *)
(*
infinite_na x t = (infinite_nb x t, x, nil)
infinite_nb x t = (infinite_na x t, x, nil)
*)
