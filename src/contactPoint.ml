type t = {
  distance:float;
  pointA: Vecmath.Vector.t; pointB:Vecmath.Vector.t;
  normal:Vecmath.Vector.t;
  constraints: Constraint.t array;
}
