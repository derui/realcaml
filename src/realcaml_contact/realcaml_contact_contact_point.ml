type t = {
  distance:float;
  pointA: Types.vec;
  pointB:Types.vec;
  normal:Types.vec;
  constraints: Constraint.t list;
}

module U = Realcaml_util.Vec
let empty =
  {distance = 0.0; pointA = U.empty; pointB = U.empty;
   normal = U.empty; constraints = []}
