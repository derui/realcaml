(**
   Contact Point is one of the contacted point between two rigid bodies.

   @version 0.1
   @author derui
*)

type t = {
  distance:float;
  pointA: Types.vec;
  pointB:Types.vec;
  normal:Types.vec;
  constraints: Constraint.t list;
}

val empty : t
(** Get empty contact point *)
