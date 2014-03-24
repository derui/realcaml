type t = {
  inertia:Candyvec.Matrix.t;
  mass:float;
  restitution:float;
  friction:float;
}

let empty = {inertia = Candyvec.Matrix.identity ();mass = 0.0;
             restitution = 0.0; friction = 0.0}
