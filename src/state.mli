(**
   Providing types are to keep some state for rigid body.

   @version 0.1
   @author derui
*)

type motion = Active | Static

(** Types including some statements of rigid body. *)
type t

(** [make ~pos ~orient ~linear ~angular ~motion_type] make a status for
    rigid body in physics simulations.
*)
val make: pos:Vecmath.Vector.t -> orient:Vecmath.Quaternion.t ->
  linear:Vecmath.Vector.t -> angular:Vecmath.Vector.t -> motion_type:motion -> t

(** Get a empty state. *)
val empty: t

(** Get current position  *)
val pos : t -> Vecmath.Vector.t

(** Get current orientation  *)
val orientation: t -> Vecmath.Quaternion.t

(** Get current linear velocity  *)
val linear_velocity: t -> Vecmath.Vector.t

(** Get current angular velocity  *)
val angular_velocity: t -> Vecmath.Vector.t

(** Get motion type of a rigid body  *)
val motion_type: t -> motion
