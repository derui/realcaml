(**
   This module provide physical attributes for a Rigid Body and operations.
   Providing attributes are inertia, mass, restitution and friction of a Rigid Body.

   @version 0.1
   @author derui
*)

(** type of rigid body  *)
type t = {
  inertia:Types.mat;
  mass:float;
  restitution:float;
  friction:float;
}

(** Get a rigid body to empty all information. *)
val empty : t
