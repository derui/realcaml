(**
   Contact Point is one of the contacted point between two rigid bodies.

   @version 0.1
   @author derui
*)

type t = {
  distance:float;
  pointA: Candyvec.Vector.t; pointB:Candyvec.Vector.t;
  normal:Candyvec.Vector.t;
  constraints: Constraint.t array;
}
