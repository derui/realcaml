(**
   Contact Point is one of the contacted point between two rigid bodies.

   @version 0.1
   @author derui
*)

type t = {
  distance:float;
  pointA: Candyvec.Std.Vector.t;
  pointB:Candyvec.Std.Vector.t;
  normal:Candyvec.Std.Vector.t;
  constraints: Constraint.t list;
}

val empty : t
(** Get empty contact point *)
