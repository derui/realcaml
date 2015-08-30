
type vec = Realcaml_util.Types.vec
type mat = Realcaml_util.Types.mat

module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size

type three_vec = S.three S.t A.vec

type triangle = three_vec * three_vec * three_vec
(* The type of triangle. *)
