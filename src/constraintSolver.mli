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
  type t = {delta_linear_velocity: Vecmath.Vector.t;
            delta_angular_velocity: Vecmath.Vector.t;
            orientation:Vecmath.Quaternion.t;
            inertia_inv:Vecmath.Matrix3.t;
            mass_inv:float;
           }

  (** Get empty solver body  *)
  val empty: t

  (** Set up new solver body with a state of the rigid body *)
  val setup : State.t -> RigidBody.t -> t
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
