type t = {
  axis:Candyvec.Vector.t;
  jac_diag_inv:float;
  rhs:float;
  lower_limit:float;
  upper_limit:float;
  accum_impulse:float;
}
