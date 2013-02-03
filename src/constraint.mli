(**
   Constraint contains parameters for calculating constraint.

   @version 0.1
   @author derui
*)

type t

(** Make and return Constraint data by given arguments  *)
val make: axis:Vecmath.Vector.t -> inv:float ->
  rhs:float -> lower:float -> upper:float -> accum:float -> t

(** Get constraint axis *)
val axis: t -> Vecmath.Vector.t

(** Get the denominator of constraint expression*)
val jac_diag_inv: t -> float

(** Get the initial power of constraint *)
val rhs: t -> float

(** Get lower limit of constraint power  *)
val lower_limit: t -> float

(** Get upper limit of constraint power  *)
val upper_limit: t -> float

(** Get the constraint power is accumulated by impulse. *)
val accum_impulse: t -> float
