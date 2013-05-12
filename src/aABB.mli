(**
   Containing utility functions for AABB, for instance, intersect each other,
   to calculate AABB.

   AABB is 'Axis arigned bounding box'.

   @version 0.1
   @author derui
*)

(** A `AABB` type.  *)
type t = {
  center:Vecmath.Vector.t;
  half_size: Vecmath.Vector.t
}

(** Check intersection each AABB.  *)
val intersect: t -> t -> bool

(** Check intersection in only one axis.
    Order of checking intersection is A to B.
*)
val intersect_one_axis: pos_a:float -> len_a:float ->
  pos_b:float -> len_b:float -> bool
