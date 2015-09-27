(**
   Collidable is container for some {! Shape}s and has AABB to detect collision
   between rigid bodies.

   @version 0.1
   @author derui
*)


(** Type of collidable. *)
type t = {
  (** The shapes in given collidable container.  *)
  shapes: Realcaml_rigid_body_shape.t array;
  (** Center of the AABB *)
  center: Realcaml_util.vec;
  (** The half size of the AABB from center of it.  *)
  half_size: Realcaml_util.vec;
}

(** Get empty collidable. *)
val empty : t

val build : Realcaml_rigid_body_shape.t array -> t
(** Build AABB fitting own shapes and return including it. *)

val rebuild : t -> t
(** Rebuild AABB fitting shapes in the given Collidable.t and return new it. *)
