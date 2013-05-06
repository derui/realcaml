(**
   A 3-axis sweep and prune algorism implementation.

   @version 0.1
   @author derui
*)

(** data type to manage sweep and prune.  *)
type t

(** Make sweep and prune management data. An argument is number of
    collidable be able to contain it.
*)
val make : int -> t

val add : t -> RigidBodyInfo.t -> t

(** Get max number of infomation to be able to contain in.  *)
val get_max_count: t -> int

(** check intersecting two rigid body.
    If two rigid body intersect each other, return two rigid body information,
    then them order is first - second that ordering are in arguments.
*)
val intersect: t -> int -> int -> (RigidBodyInfo.t * RigidBodyInfo.t) option

(** Get RigidBodyInfo at given index in the SweepPrune.
*)
val get_body : t -> int -> RigidBodyInfo.t option

(** Set RigidBodyInfo at given index in the SeepPlune *)
val set_body : t -> int -> RigidBodyInfo.t -> t
