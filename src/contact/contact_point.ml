module U = Realcaml_util

type t = {
  distance : float;
  pointA : U.Types.vec;
  pointB : U.Types.vec;
  normal : U.Types.vec;
  constraints : Constraint.t list;
}

let empty () =
  { distance = 0.0; pointA = U.Vec.empty (); pointB = U.Vec.empty (); normal = U.Vec.empty (); constraints = [] }
