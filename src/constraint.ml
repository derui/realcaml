type t = {
  axis:Vecmath.Vector.t;
  jac_diag_inv:float;
  rhs:float;
  lower_limit:float;
  upper_limit:float;
  accum_impulse:float;
}

let make ~axis ~inv ~rhs ~lower ~upper ~accum =
  {axis; jac_diag_inv = inv; rhs; lower_limit = lower;
   upper_limit = upper; accum_impulse = accum}

let axis {axis;_} = axis

let jac_diag_inv {jac_diag_inv;_} = jac_diag_inv

let rhs {rhs;_} = rhs

let lower_limit {lower_limit;_} = lower_limit

let upper_limit {upper_limit;_} = upper_limit

let accum_impulse {accum_impulse;_} = accum_impulse
