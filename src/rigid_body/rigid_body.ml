module A = Typedvec.Algebra
module S = Typedvec.Size
module U = Realcaml_util

type t = {
  inertia : U.Types.mat;
  mass : float;
  restitution : float;
  friction : float;
}

let empty = { inertia = A.Mat.identity S.four; mass = 0.0; restitution = 0.0; friction = 0.0 }
