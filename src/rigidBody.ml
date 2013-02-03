type t = {
  inertia:Vecmath.Matrix3.t;
  mass:float;
  restitution:float;
  friction:float;
}

let make ~inertia ~mass ~restitution ~friction =
  {inertia;mass;restitution; friction}

let empty = {inertia = Vecmath.Matrix3.identity ();mass = 0.0;
             restitution = 0.0; friction = 0.0}

let inertia {inertia;_} = inertia

let mass {mass;_} = mass

let restitution {restitution;_} = restitution

let friction {friction;_} = friction
