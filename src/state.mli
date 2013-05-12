(**
   Providing types are to keep some state for rigid body.

   @version 0.1
   @author derui
*)

type motion = Active | Static

(** Types including some statements of rigid body. *)
type t = {
  pos:Vecmath.Vector.t;
  orientation:Vecmath.Quaternion.t;
  linear_velocity:Vecmath.Vector.t;
  angular_velocity:Vecmath.Vector.t;
  motion_type:motion;
}

(** Get a empty state. *)
val empty: t
