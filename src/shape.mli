(**
   Shape provide to access real geometory and offsets.
   This is always used with {!Collidable}.

   @version 0.1
   @author derui
*)

(** type of shape  *)
type t = {
  mesh:Mesh.t;
  offset_pos:Vecmath.Vector.t;
  offset_orientation:Vecmath.Quaternion.t;
}

(** Get offset transformation matrix of given shape  *)
val offset_transform : t -> Vecmath.Matrix4.t
