(**
   A 3-axis sweep and prune algorism implementation.

   @version 0.1
   @author derui
*)

(** data type to manage sweep and prune.  *)
type t = {
  bodies:Realcaml_rigid_body.Rigid_body_info.t option array;
  current_count: int;
}

type intersect = Realcaml_rigid_body.Rigid_body_info.t * Realcaml_rigid_body.Rigid_body_info.t

(** Make sweep and prune management data. An argument is number of
    collidable be able to contain it.
*)
val make : int -> t

val add : t -> Realcaml_rigid_body.Rigid_body_info.t -> t

(** check intersecting two rigid body.
    If two rigid body intersect each other, return two rigid body information,
    then them order is first - second that ordering are in arguments.
*)
val intersect: t -> int -> int -> intersect option
