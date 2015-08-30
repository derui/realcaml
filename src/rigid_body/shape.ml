module A = Typedvec.Std.Algebra
module M = Realcaml_mesh.Mesh

type t = {
  mesh:M.t;
  offset_pos: Types.vec;
  offset_orientation:Typedvec.Std.Ext.Qua.t;
}

let offset_transform {offset_pos; offset_orientation;_} =
  Realcaml_util.Util.to_world_transform offset_pos offset_orientation
