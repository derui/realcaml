module A = Typedvec.Std.Algebra
module V = A.Vec
module M = A.Mat
module U = Realcaml_util

type t = {
  axis:U.vec;
  jac_diag_inv:float;
  rhs:float;
  lower_limit:float;
  upper_limit:float;
  accum_impulse:float;
}

let make info axis =
  let open V.Open in
  let open A.Open in
  let open Realcaml_contact_setup in
  let denom = ((U.Vec.to_four axis) *> info.k |> U.Vec.to_three) *: axis in
  let rhs = -.(1.0 +. info.restriction) *. (info.relative_velocity *: axis) in
  let rhs = rhs -.
            (info.contact_bias *. min 0.0 info.distance) /. info.time_step in
  let jac_diag_inv = 1.0 /. denom in
  let rhs = rhs *. jac_diag_inv in
  {axis; jac_diag_inv; rhs;
   lower_limit = 0.0;
   upper_limit = max_float;
   accum_impulse = 0.0
  }
