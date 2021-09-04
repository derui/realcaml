(**
   Integrated Rigid body's infomations which are contained Collidable,
   RigidBody,and State.
   It is used by sweep and plune to detect collision, and so on.

   @version 0.1
   @derui
*)


type t = {
  body:Rigid_body.t;
  (** The rigid body *)

  collidable:Collidable.t;
  (** the collidable information of rigid body *)

  state:State.t;
  (** the state of the rigid body *)
}

(** Get transformation matrix to world coodinates *)
val get_world_transform: t -> Realcaml_util.Types.mat

(** Get position of the RigidBodyInfo *)
val pos : t -> Realcaml_util.Types.vec

(** Set position of the RigidBodyInfo *)
val set_pos : t -> pos:Realcaml_util.Types.vec -> t
