type t = {
  distance:float;
  pointA: Candyvec.Vector.t; pointB:Candyvec.Vector.t;
  normal:Candyvec.Vector.t;
  constraints: Constraint.t array;
}
