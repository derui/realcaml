(**
   Constraint contains parameters for calculating constraint.

   @version 0.1
   @author derui
*)

type t = {
  (** constraint axis  *)
  axis:Candyvec.Std.Vector.t;
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

module Setup : sig
  type t = {
    k : Candyvec.Std.Matrix.t;
  (** The k matrix that is calculated to physical quantity of constraint *)
    restriction : float;
    (** The restriction between two bodies to calculate constraint *)
    relative_velocity : Candyvec.Std.Vector.t;
  (** relative velocity between two bodies. *)
    contact_bias : float;
  (** The bias for distance to detect bodies are not contact *)
    distance: float;
  (** The distance of contacted point between two bodies. *)
    time_step : float;
  (** The time step from [!Engine_option]. *)
  }

end

val make : Setup.t -> Candyvec.Std.Vector.t -> t
(** [!setup setup_info axis] to setup the Constraint for axis. *)
