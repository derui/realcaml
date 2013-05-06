
(* constant value of pi. *)
let pi = acos (-1.0)

let rad_to_deg rad = rad *. (pi /. 180.0)
let deg_to_rad deg = deg *. (180.0 /. pi)
