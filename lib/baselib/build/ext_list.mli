(** Extention for List.
    Contained some extentional functions for {!List},
    and utility for List.

    All functions in this module are integrated with {!List} where
    be in {!Std} module.
*)

(** return list within the compass of given min to given max. *)
val range : Num.num * Num.num -> Num.num list

(** range specified for int. detail behavior equals {!range} *)
val range_int : int * int -> int list

(** range of float values are specified by each value step, start and end of range.  *)
val range_float : step:float -> fromto:float * float -> float list

(**
   initialization list. this return list that applied each \[0..n) to
   f. it likes \[f(0);f(1);..f(n-1)\]
*)
val init : int -> (int -> 'a) -> 'a list

(** return list within the compass of given min to given max.
    This function is alias to {!range_int}
*)
val (--) : int -> int -> int list

(** This function is alias to {!range}
*)
val (--/) : Num.num -> Num.num -> Num.num list
