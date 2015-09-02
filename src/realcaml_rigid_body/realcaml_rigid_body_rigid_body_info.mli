(**
   Integrated Rigid body's infomations which are contained Collidable,
   RigidBody,and State.
   It is used by sweep and plune to detect collision, and so on.

   @version 0.1
   @derui
*)

type t = {
  (** The rigid body *)
  body:Realcaml_rigid_body_rigid_body.t;
  (** the collidable information of rigid body *)
  collidable:Realcaml_rigid_body_collidable.t;
  (** the state of the rigid body *)
  state:Realcaml_rigid_body_state.t;
}

(** Get transformation matrix to world coodinates *)
val get_world_transform: t -> Realcaml_util.mat

(** Get position of the RigidBodyInfo *)
val pos : t -> Realcaml_util.vec

(** Set position of the RigidBodyInfo *)
val set_pos : t -> pos:Realcaml_util.vec -> t
