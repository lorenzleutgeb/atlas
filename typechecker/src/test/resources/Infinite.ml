(** Contains function definitions that do not terminate. *)

infinite_1 x = (infinite_1 x, x, infinite_1 x)

infinite_2 x t = (infinite_2 x t, x, t)
