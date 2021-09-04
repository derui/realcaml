module A = Typedvec.Algebra
module M = Realcaml_mesh.Mesh
module U = Realcaml_util

type t = {
  mesh : M.t;
  offset_pos : U.Types.vec;
  offset_orientation : Typedvec.Ext.Qua.t;
}

let offset_transform { offset_pos; offset_orientation; _ } = U.Fn.to_world_transform offset_pos offset_orientation
