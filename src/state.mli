(**
   Providing types are to keep some state for rigid body.

   @version 0.1
   @author derui
*)

type motion = Active | Static

(** Types including some statements of rigid body. *)
type t = {
  pos:Candyvec.Std.Vector.t;
  orientation:Candyvec.Std.Quaternion.t;
  linear_velocity:Candyvec.Std.Vector.t;
  angular_velocity:Candyvec.Std.Vector.t;
  motion_type:motion;
}

(** Get a empty state. *)
val empty: t

(** Get a transform matrix to convert world coodinate. *)
val to_world_transform : t -> Candyvec.Std.Matrix4.t
