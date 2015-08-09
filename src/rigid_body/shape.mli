(**
   Shape provide to access real geometory and offsets.
   This is always used with {!Collidable}.

   @version 0.1
   @author derui
*)

module M = Realcaml_mesh.Mesh

(** type of shape  *)
type t = {
  mesh: M.t;
  offset_pos:Types.vec;
  offset_orientation:Typedvec.Std.Ext.Qua.t;
}

(** Get offset transformation matrix of given shape  *)
val offset_transform : t -> Types.mat
