(**
   Containing utility functions for AABB, for instance, intersect each other,
   to calculate AABB.

   AABB is 'Axis arigned bounding box'.

   @version 0.1
   @author derui
*)

(** A `AABB` type.  *)
type t = {
  center: Realcaml_util_types.vec;
  half_size: Realcaml_util_types.vec;
}

val empty: unit -> t
(* [empty ()] return a empty AABB type *)

val make: center:Realcaml_util_types.vec -> half_size:Realcaml_util_types.vec -> unit -> t
(* [make ~center ~half_size ()] get a AABB box having [center] and [half_size]. *)

(** Check intersection each AABB.  *)
val intersect: t -> t -> bool
