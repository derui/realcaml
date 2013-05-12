(**
   Collidable is container for some {! Shape}s and has AABB to detect collision
   between rigid bodies.

   @version 0.1
   @author derui
*)

(** Type of collidable. *)
type t = {
  (** The shapes in given collidable container.  *)
  shapes:Shape.t array;
  (** Center of the AABB *)
  center: Vecmath.Vector.t;
  (** The half size of the AABB from center of it.  *)
  half_size: Vecmath.Vector.t;
}

(** Get empty collidable. *)
val empty : t
