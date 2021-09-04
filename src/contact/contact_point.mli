(** Contact Point is one of the contacted point between two rigid bodies.

    @version 0.1
    @author derui *)

type t = {
  distance : float;
  pointA : Realcaml_util.Types.vec;
  pointB : Realcaml_util.Types.vec;
  normal : Realcaml_util.Types.vec;
  constraints : Constraint.t list;
}

val empty : unit -> t
(** Get empty contact point *)
