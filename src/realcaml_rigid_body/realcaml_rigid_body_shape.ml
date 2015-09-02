module A = Typedvec.Std.Algebra
module M = Realcaml_mesh.Mesh
module U = Realcaml_util

type t = {
  mesh:M.t;
  offset_pos: U.vec;
  offset_orientation:Typedvec.Std.Ext.Qua.t;
}

let offset_transform {offset_pos; offset_orientation;_} =
  U.to_world_transform offset_pos offset_orientation
