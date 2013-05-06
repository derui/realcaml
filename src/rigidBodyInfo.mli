(**
   Integrated Rigid body's infomations which are contained Collidable,
   RigidBody,and State.
   It is used by sweep and plune to detect collision, and so on.

   @version 0.1
   @derui
*)

type t

(** Make a rigid body information to use with sweep and prune.

    @param body The rigid body
    @param col the collidable information of rigid body
    @param state the state of the rigid body
    @return rigid body information
*)
val make : body:RigidBody.t -> col:Collidable.t -> state:State.t -> t

(** Get rigid body in a rigid body information  *)
val rigid_body: t -> RigidBody.t

(** Get collidable of a rigid body in a rigid body information  *)
val collidable: t -> Collidable.t

(** Get state of a rigid boyd in a rigid body information  *)
val state: t -> State.t

(** Get new ribid body information updated by new State.t  *)
val set_state:t -> State.t -> t

(** Get transformation matrix to world coodinates *)
val get_world_transform: t -> Vecmath.Matrix4.t
