open Candyvec

type t = {
  mesh:Mesh.t;
  offset_pos:Candyvec.Vector.t;
  offset_orientation:Candyvec.Quaternion.t;
}

let offset_transform {offset_pos; offset_orientation;_} =
  let trans_mat = Matrix.translation offset_pos
  and orient_mat = Quaternion.to_matrix offset_orientation in
  Matrix.multiply trans_mat orient_mat
