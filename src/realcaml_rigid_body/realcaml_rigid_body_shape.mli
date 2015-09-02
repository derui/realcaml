(**
   Shape provide to access real geometory and offsets.
   This is always used with {!Collidable}.

   @version 0.1
   @author derui
*)

(** type of shape  *)
type t = {
  mesh: Realcaml_mesh.Mesh.t;
  offset_pos:Realcaml_util.vec;
  offset_orientation:Typedvec.Std.Ext.Qua.t;
}

(** Get offset transformation matrix of given shape  *)
val offset_transform : t -> Realcaml_util.mat
