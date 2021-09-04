(** Constraint contains parameters for calculating constraint.

    @version 0.2
    @author derui *)

type t = {
  axis : Realcaml_util.Types.vec;  (** constraint axis *)
  jac_diag_inv : float;  (** the denominator of constraint expression *)
  rhs : float;  (** the initial power of constraint *)
  lower_limit : float;  (** lower limit of constraint power *)
  upper_limit : float;  (** upper limit of constraint power *)
  accum_impulse : float;  (** the constraint power is accumulated by impulse *)
}

val make : Setup.t -> Realcaml_util.Types.vec -> t
(** [!setup setup_info axis] to setup the Constraint for axis. *)
