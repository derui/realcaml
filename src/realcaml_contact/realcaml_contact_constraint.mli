(**
   Constraint contains parameters for calculating constraint.

   @version 0.2
   @author derui
*)

type t = {
  (** constraint axis  *)
  axis:Realcaml_util.vec;
  (** the denominator of constraint expression  *)
  jac_diag_inv:float;
  (** the initial power of constraint *)
  rhs:float;
  (** lower limit of constraint power  *)
  lower_limit:float;
  (** upper limit of constraint power  *)
  upper_limit:float;
  (** the constraint power is accumulated by impulse *)
  accum_impulse:float;
}

val make : Realcaml_contact_setup.t -> Realcaml_util.vec -> t
(** [!setup setup_info axis] to setup the Constraint for axis. *)
