(** This module provide solver for constraint and helper functions.

    @author derui
    @version 0.1 *)

(** Contain for solving constraint between rigid bodies. *)
module Solver_body : sig
  type t = {
    delta_linear_velocity : Realcaml_util.Types.vec;
    delta_angular_velocity : Realcaml_util.Types.vec;
    orientation : Typedvec.Ext.Qua.t;
    inertia_inv : Realcaml_util.Types.mat;
    mass_inv : float;
    accum_impulse : float;
  }
  (** type of the SolverBody module *)

  val empty : t
  (** Get new empty solver body *)

  val setup : state:Realcaml_rigid_body.State.t -> body:Realcaml_rigid_body.Rigid_body.t -> unit -> t
  (** Set up new solver body with a state of the rigid body *)
end

(* Options to setup constraint solver. *)
type constraint_option = {
  contact_bias : float;
  time_step : float;
}

type solver_info = Realcaml_rigid_body.Rigid_body_info.t * Solver_body.t

val setup_solver_body : Realcaml_rigid_body.Rigid_body_info.t -> solver_info
(** Create new SolverBody with the rigid body information given *)

val setup_constraint : solver_info -> solver_info -> Contact.t -> Pair.pair -> constraint_option -> Contact.t
(** Setup constraints in contact point for between given solver_info *)

val solve : solver_info -> solver_info -> Contact.t -> solver_info * solver_info
(** Solve constraint power with solver body *)

val update_constraint : Contact.t -> solver_info -> Contact.t
(** return new Contact that is updated to be updating constraints of given Contact *)
