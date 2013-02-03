(**
   This module provide physical attributes for a Rigid Body and operations.
   Providing attributes are inertia, mass, restitution and friction of a Rigid Body.

   @version 0.1
   @author derui
*)

(** type of rigid body  *)
type t

(** Make rigid body by some parameter for it.

    @param inertia the inertia of made rigid body from this.
    @param mass the mass of made rigid body from this.
    @param restitution the restitution of made rigid body from this.
    @param friction the friction of made rigid body from this.
    @return rigid body initialized by given parameters.
*)
val make : inertia:Vecmath.Matrix3.t ->
  mass:float -> restitution:float -> friction:float -> t

(** Get a rigid body to empty all information. *)
val empty : t

(** Get inertia of given rigid body  *)
val inertia : t -> Vecmath.Matrix3.t

(** Get mass of given rigid body   *)
val mass : t -> float

(** Get restitution of given rigid body  *)
val restitution : t -> float

(** Get friction of given rigid body  *)
val friction: t -> float
