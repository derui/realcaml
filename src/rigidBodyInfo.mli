(**
   Integrated Rigid body's infomations which are contained Collidable,
   RigidBody,and State.
   It is used by sweep and plune to detect collision, and so on.

   @version 0.1
   @derui
*)

type t = {
  (** The rigid body *)
  body:RigidBody.t;
  (** the collidable information of rigid body *)
  collidable:Collidable.t;
  (** the state of the rigid body *)
  state:State.t;
}

(** Get transformation matrix to world coodinates *)
val get_world_transform: t -> Candyvec.Matrix4.t

(** Set position of the RigidBodyInfo *)
val set_pos : t -> Candyvec.Vector.t -> t
