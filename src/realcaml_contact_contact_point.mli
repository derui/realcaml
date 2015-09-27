(**
   Contact Point is one of the contacted point between two rigid bodies.

   @version 0.1
   @author derui
*)

type t = {
  distance:float;
  pointA: Realcaml_util.vec;
  pointB:Realcaml_util.vec;
  normal:Realcaml_util.vec;
  constraints: Realcaml_contact_constraint.t list;
}

val empty : unit -> t
(** Get empty contact point *)
