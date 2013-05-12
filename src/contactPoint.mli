(**
   Contact Point is one of the contacted point between two rigid bodies.

   @version 0.1
   @author derui
*)

type t = {
  distance:float;
  pointA: Vecmath.Vector.t; pointB:Vecmath.Vector.t;
  normal:Vecmath.Vector.t;
  constraints: Constraint.t array;
}
