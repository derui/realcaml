type t = {
  inertia:Candyvec.Matrix3.t;
  mass:float;
  restitution:float;
  friction:float;
}

let empty = {inertia = Candyvec.Matrix3.identity ();mass = 0.0;
             restitution = 0.0; friction = 0.0}
