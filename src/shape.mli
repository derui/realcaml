(**
   Shape provide to access real geometory and offsets.
   This is always used with {!Collidable}.

   @version 0.1
   @author derui
*)

(** type of shape  *)
type t

(** Make initialized shape by given parameters  *)
val make : mesh:Mesh.t -> pos:Vecmath.Vector.t -> orient:Vecmath.Quaternion.t -> t

(** Get real mesh of the shape.  *)
val mesh: t -> Mesh.t

(** Get offset position of given shape  *)
val offset_pos : t -> Vecmath.Vector.t

(** Get offset orientation of given shape  *)
val offset_orientation : t -> Vecmath.Quaternion.t
