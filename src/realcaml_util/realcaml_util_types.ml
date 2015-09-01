
module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size

type vec = S.three S.t A.vec
type mat = (S.four S.t, S.four S.t) A.mat

type four_vec = S.four S.t A.vec
