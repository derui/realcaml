type t = {
  inertia:Vecmath.Matrix3.t;
  mass:float;
  restitution:float;
  friction:float;
}

let empty = {inertia = Vecmath.Matrix3.identity ();mass = 0.0;
             restitution = 0.0; friction = 0.0}
