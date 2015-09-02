(**
   Providing types are to keep some state for rigid body.

   @version 0.1
   @author derui
*)

type motion = Active | Static

(** Types including some statements of rigid body. *)
type t = {
  pos:Realcaml_util.vec;
  orientation:Typedvec.Std.Ext.Qua.t;
  linear_velocity:Realcaml_util.vec;
  angular_velocity:Realcaml_util.vec;
  motion_type:motion;
}

(** Get a empty state. *)
val empty: t

(** Get a transform matrix to convert world coodinate. *)
val to_world_transform : t -> Realcaml_util.mat

(** A shortcut to check motion type of a State. *)
val is_static : t -> bool
val is_active : t -> bool
