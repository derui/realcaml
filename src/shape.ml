open Vecmath

type t = {
  mesh:Mesh.t;
  offset_pos:Vecmath.Vector.t;
  offset_orientation:Vecmath.Quaternion.t;
}

let make ~mesh ~pos ~orient =
  {mesh; offset_pos = pos; offset_orientation = orient}

(** Get real mesh of the shape.  *)
let mesh {mesh;_} = mesh

let offset_pos {offset_pos;_} = offset_pos

let offset_orientation {offset_orientation;_} = offset_orientation

let offset_transform {offset_pos; offset_orientation;_} =
  let trans_mat = Matrix4.translation offset_pos
  and orient_mat = Quaternion.to_matrix offset_orientation in
  Matrix4.multiply trans_mat orient_mat
