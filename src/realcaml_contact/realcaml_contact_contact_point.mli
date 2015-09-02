(**
   Contact Point is one of the contacted point between two rigid bodies.

   @version 0.1
   @author derui
*)

module U = Realcaml_util

type t = {
  distance:float;
  pointA: U.vec;
  pointB:U.vec;
  normal:U.vec;
  constraints: Constraint.t list;
}

val empty : t
(** Get empty contact point *)
