type t = {
  axis:Candyvec.Std.Vector.t;
  jac_diag_inv:float;
  rhs:float;
  lower_limit:float;
  upper_limit:float;
  accum_impulse:float;
}

module Setup = struct
  type t = {
    k : Candyvec.Std.Matrix.t;
    restriction : float;
    relative_velocity : Candyvec.Std.Vector.t;
    contact_bias : float;
    distance: float;
    time_step : float;
  }
end

module V = Candyvec.Std.Vector
module M = Candyvec.Std.Matrix

let make info axis =
  let open V.Open in
  let open M.Open in
  let open Setup in
  let denom = V.dot (axis *||> info.k) axis in
  let rhs = -.(1.0 +. info.restriction) *. (info.relative_velocity *@ axis) in
  let rhs = rhs -.
    (info.contact_bias *. min 0.0 info.distance) /. info.time_step in
  let jac_diag_inv = 1.0 /. denom in
  let rhs = rhs *. jac_diag_inv in
  {axis; jac_diag_inv; rhs;
   lower_limit = 0.0;
   upper_limit = max_float;
   accum_impulse = 0.0
  }
