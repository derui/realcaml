(** This module provide physical attributes for a Rigid Body and operations. Providing attributes are inertia, mass,
    restitution and friction of a Rigid Body.

    @version 0.1
    @author derui *)

type t = {
  inertia : Realcaml_util.Types.mat;
  mass : float;
  restitution : float;
  friction : float;
}
(** type of rigid body *)

val empty : t
(** Get a rigid body to empty all information. *)
