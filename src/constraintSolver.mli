(**
   This module provide solver for constraint and helper functions.

   @author derui
   @version 0.1
*)


(**
   Contain for solving constraint between rigid bodies.
*)
module SolverBody : sig

  (** type of the SolverBody module  *)
  type t

  (** Get empty solver body  *)
  val empty: t

  (** Set up new solver body with a state of the rigid body *)
  val setup : State.t -> RigidBody.t -> t

  (** get delta of linear velocity of given solver body *)
  val delta_linear_velocity : t -> Vecmath.Vector.t

  (** get delta of angular velocity of given solver body *)
  val delta_angular_velocity : t -> Vecmath.Vector.t

  (** get the orientation of given solver body *)
  val orientation : t -> Vecmath.Quaternion.t

  (** get the inverse for inertia of given solver body  *)
  val inertia_inv : t -> Vecmath.Matrix3.t

  (** get the inverse for mass of given solver body *)
  val mass_inv : t -> float
end

(** Create new SolverBody with the rigid body information given *)
val setup_solver_body : RigidBodyInfo.t -> SolverBody.t

type solver_info = RigidBodyInfo.t * SolverBody.t

(** Setup constraints in contact point for between given solver_info   *)
val setup_constraint : solver_info -> solver_info -> Contact.t -> Pair.pair ->
  Engine_option.engine_option -> Contact.t

(** Solve constraint power with solver body *)
val solve : solver_info -> solver_info -> Contact.t -> Engine_option.engine_option ->
  solver_info * solver_info

(** return new Contact that is updated to be updating constraints of given Contact  *)
val update_constraint : Contact.t -> Contact.t
