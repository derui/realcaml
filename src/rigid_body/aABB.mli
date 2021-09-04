(** Containing utility functions for AABB, for instance, intersect each other, to calculate AABB.

    AABB is 'Axis arigned bounding box'.

    @version 0.1
    @author derui *)

type t = {
  center : Realcaml_util.Types.vec;
  half_size : Realcaml_util.Types.vec;
}
(** A `AABB` type. *)

val empty : unit -> t
(* [empty ()] return a empty AABB type *)

val make : center:Realcaml_util.Types.vec -> half_size:Realcaml_util.Types.vec -> unit -> t
(* [make ~center ~half_size ()] get a AABB box having [center] and [half_size]. *)

val intersect : t -> t -> bool
(** Check intersection each AABB. *)
