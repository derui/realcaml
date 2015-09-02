(**
   Collidable is container for some {! Shape}s and has AABB to detect collision
   between rigid bodies.

   @version 0.1
   @author derui
*)

module M = Realcaml_mesh
module U = Realcaml_util

(** Type of collidable. *)
type t = {
  (** The shapes in given collidable container.  *)
  shapes: M.Shape.t array;
  (** Center of the AABB *)
  center: U.vec;
  (** The half size of the AABB from center of it.  *)
  half_size: U.vec;
}

(** Get empty collidable. *)
val empty : t

val build : M.Shape.t array -> t
(** Build AABB fitting own shapes and return including it. *)

val rebuild : t -> t
(** Rebuild AABB fitting shapes in the given Collidable.t and return new it. *)
