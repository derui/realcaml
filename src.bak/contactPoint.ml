type t = {
  distance:float;
  pointA: Candyvec.Std.Vector.t; pointB:Candyvec.Std.Vector.t;
  normal:Candyvec.Std.Vector.t;
  constraints: Constraint.t list;
}

module V = Candyvec.Std.Vector
let empty =
  {distance = 0.0; pointA = V.zero; pointB = V.zero;
   normal = V.zero; constraints = []}
