(**
   Contact Point is one of the contacted point between two rigid bodies.

   @version 0.1
   @author derui
*)

type t

(** Make Constact Point data containing given arguments.  *)
val make: dist:float -> pointA:Vecmath.Vector.t -> pointB:Vecmath.Vector.t ->
  normal:Vecmath.Vector.t -> constraints:Constraint.t array -> t

(** Get penetrated distance at the contact point *)
val distance: t -> float

(** Get contact point in local coodinates of the rigid body is named A. *)
val pointA: t -> Vecmath.Vector.t

(** Get contact point in local coodinates of the rigid body is named B. *)
val pointB: t -> Vecmath.Vector.t

(** Get normal vector of contact point in world coodinates.  *)
val normal: t -> Vecmath.Vector.t

(** Get constraints of given contact point. *)
val constraints: t -> Constraint.t array
