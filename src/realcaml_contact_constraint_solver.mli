(**
   This module provide solver for constraint and helper functions.

   @author derui
   @version 0.1
*)

(**
   Contain for solving constraint between rigid bodies.
*)
module Solver_body : sig

  (** type of the SolverBody module  *)
  type t = {
    delta_linear_velocity: Realcaml_util.vec;
    delta_angular_velocity: Realcaml_util.vec;
    orientation:Typedvec.Ext.Qua.t;
    inertia_inv:Realcaml_util.mat;
    mass_inv:float;
    accum_impulse : float;
  }

  (** Get new empty solver body  *)
  val empty: t

  (** Set up new solver body with a state of the rigid body *)
  val setup : state:Realcaml_rigid_body_state.t -> body:Realcaml_rigid_body_rigid_body.t -> unit -> t
end

(* Options to setup constraint solver. *)
type constraint_option = {
  contact_bias: float;
  time_step: float;
}

type solver_info = Realcaml_rigid_body_rigid_body_info.t * Solver_body.t

(** Create new SolverBody with the rigid body information given *)
val setup_solver_body : Realcaml_rigid_body_rigid_body_info.t -> solver_info 

(** Setup constraints in contact point for between given solver_info   *)
val setup_constraint : solver_info -> solver_info -> Realcaml_contact_contact.t ->
  Realcaml_contact_pair.pair -> constraint_option -> Realcaml_contact_contact.t

(** Solve constraint power with solver body *)
val solve : solver_info -> solver_info -> Realcaml_contact_contact.t -> solver_info * solver_info

(** return new Contact that is updated to be updating constraints of given Contact  *)
val update_constraint : Realcaml_contact_contact.t -> solver_info -> Realcaml_contact_contact.t
