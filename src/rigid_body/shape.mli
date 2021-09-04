(** Shape provide to access real geometory and offsets. This is always used with {!Collidable}.

    @version 0.1
    @author derui *)

type t = {
  mesh : Realcaml_mesh.Mesh.t;
  offset_pos : Realcaml_util.Types.vec;
  offset_orientation : Typedvec.Ext.Qua.t;
}
(** type of shape *)

val offset_transform : t -> Realcaml_util.Types.mat
(** Get offset transformation matrix of given shape *)
