
(* TRANSLATE: 姿勢と位置から、ワールド変換行列を作成する *)
let world_transform orient vec =
  let open Candyvec in
  let orient = Quaternion.to_matrix orient
  and trans = Matrix.translation vec in
  Matrix.multiply trans orient
