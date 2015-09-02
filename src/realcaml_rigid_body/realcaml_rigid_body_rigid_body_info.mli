(**
   Integrated Rigid body's infomations which are contained Collidable,
   RigidBody,and State.
   It is used by sweep and plune to detect collision, and so on.

   @version 0.1
   @derui
*)

module U = Realcaml_util
module Rigid_body = Realcaml_rigid_body_rigid_body
module Collidable = Realcaml_rigid_body_collidable
module State = Realcaml_rigid_body_state

type t = {
  (** The rigid body *)
  body:Rigid_body.t;
  (** the collidable information of rigid body *)
  collidable:Collidable.t;
  (** the state of the rigid body *)
  state:State.t;
}

(** Get transformation matrix to world coodinates *)
val get_world_transform: t -> U.mat

(** Get position of the RigidBodyInfo *)
val pos : t -> U.vec

(** Set position of the RigidBodyInfo *)
val set_pos : t -> pos:U.vec -> t
