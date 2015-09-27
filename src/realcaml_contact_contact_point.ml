module U = Realcaml_util
module Constraint = Realcaml_contact_constraint

type t = {
  distance:float;
  pointA: U.vec;
  pointB:U.vec;
  normal:U.vec;
  constraints: Constraint.t list;
}

let empty () =
  {distance = 0.0; pointA = U.Vec.empty (); pointB = U.Vec.empty ();
   normal = U.Vec.empty (); constraints = []}
