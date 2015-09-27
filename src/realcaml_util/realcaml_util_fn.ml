open Core.Std

let to_world_transform pos orientation =
  let module A = Typedvec.Std.Algebra in
  let module S = Typedvec.Std.Size in
  let open A.Mat.Open in
  let module Q = Typedvec.Ext.Qua in
  let module M = A.Mat in
  let module V = A.Vec in
  let module AF = Typedvec.Ext.Affine in
  let orient = Q.to_mat orientation
  and trans = Realcaml_util_vec.to_four pos |> AF.translation_to_mat in
  trans *: orient
