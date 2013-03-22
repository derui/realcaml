
(* TRANSLATE: 姿勢と位置から、ワールド変換行列を作成する *)
let world_transform orient vec =
  let open Vecmath in
  let orient = Quaternion.to_matrix orient
  and trans = Matrix4.translation vec in
  Matrix4.multiply trans orient
