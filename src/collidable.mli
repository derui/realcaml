(**
   Collidable is container for some {! Shape}s and has AABB to detect collision
   between rigid bodies.

   @version 0.1
   @author derui
*)

(** Type of collidable. *)
type t

(** Make initialized Collidable data.
    This get size of shapes, center of AABB for Shapes and size of AABB.

    @param size The size of shapes to be able to contain
    @param center the position of center of AABB
    @param half_size the half size to each axis of AABB
    @return initialized collidable data.
*)
val make: shapes:Shape.t array -> center:Vecmath.Vector.t ->
  half_size:Vecmath.Vector.t -> t

(** Get empty collidable. *)
val empty : t

(** Center of the AABB *)
val center : t -> Vecmath.Vector.t

(** The half size of the AABB from center of it.  *)
val half_size : t -> Vecmath.Vector.t

(** The shapes in given collidable container.  *)
val shapes : t -> Shape.t array
