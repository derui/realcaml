(**
   Providing types are to keep some state for rigid body.

   @version 0.1
   @author derui
*)

type motion = Active | Static

(** Types including some statements of rigid body. *)
type t = {
  pos:Candyvec.Vector.t;
  orientation:Candyvec.Quaternion.t;
  linear_velocity:Candyvec.Vector.t;
  angular_velocity:Candyvec.Vector.t;
  motion_type:motion;
}

(** Get a empty state. *)
val empty: t
