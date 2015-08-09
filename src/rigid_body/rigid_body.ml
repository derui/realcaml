module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size

type t = {
  inertia:Types.mat;
  mass:float;
  restitution:float;
  friction:float;
}


let empty = {inertia = A.Mat.identity S.four;
             mass = 0.0;
             restitution = 0.0;
             friction = 0.0}
