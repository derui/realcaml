open Vecmath

type t = {
  mesh:Mesh.t;
  offset_pos:Vecmath.Vector.t;
  offset_orientation:Vecmath.Quaternion.t;
}

let offset_transform {offset_pos; offset_orientation;_} =
  let trans_mat = Matrix4.translation offset_pos
  and orient_mat = Quaternion.to_matrix offset_orientation in
  Matrix4.multiply trans_mat orient_mat
