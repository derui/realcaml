(** Providing types are to keep some state for rigid body.

    @version 0.1
    @author derui *)

type motion =
  | Active
  | Static

type t = {
  pos : Realcaml_util.Types.vec;
  orientation : Typedvec.Ext.Qua.t;
  linear_velocity : Realcaml_util.Types.vec;
  angular_velocity : Realcaml_util.Types.vec;
  motion_type : motion;
}
(** Types including some statements of rigid body. *)

val empty : t
(** Get a empty state. *)

val to_world_transform : t -> Realcaml_util.Types.mat
(** Get a transform matrix to convert world coodinate. *)

val is_static : t -> bool
(** A shortcut to check motion type of a State. *)

val is_active : t -> bool
