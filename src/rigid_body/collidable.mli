(** Collidable is container for some {!Shape}s and has AABB to detect collision between rigid bodies.

    @version 0.1
    @author derui *)

type t = {
  shapes : Shape.t array;  (** The shapes in given collidable container. *)
  aabb : AABB.t;  (** AABB Box *)
}
(** Type of collidable. *)

val empty : t
(** Get empty collidable. *)

val build : Shape.t array -> t
(** Build AABB fitting own shapes and return including it. *)

val rebuild : t -> t
(** Rebuild AABB fitting shapes in the given Collidable.t and return new it. *)
