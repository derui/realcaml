(**
   Constraint contains parameters for calculating constraint.

   @version 0.1
   @author derui
*)

type t = {
  (** constraint axis  *)
  axis:Types.vec;
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

val make : Setup.t -> Types.vec -> t
(** [!setup setup_info axis] to setup the Constraint for axis. *)
