(*
  Closest_point provide operation and type to detect closest point between mesh and
  the point.

  @since 0.2.0
  @author derui
*)
(* result of functions in this module. That types are each positions to first and second mesh,
   and last float is distination between them.
*)
type t = {
  normal : Realcaml_util.vec;
  point_a : Realcaml_util.vec;
  point_b : Realcaml_util.vec;
  depth : float;
}

val get_closest_point: axis:Realcaml_util.vec -> dist:float -> Realcaml_rigid_body.Rigid_body_info.t ->
  Realcaml_rigid_body.Rigid_body_info.t -> t
(* [get_closest_point ~axis ~dist body_a body_b] get the closest point between [body_a] and [body_b].
   Before using this function should detect collide bodies with [Voronoi].
*)
