(**
   Providing mesh data and operations to detect collision. Provided mesh data is always convex mesh.

   @version 0.1
   @author derui
*)

(** type of convex mesh  *)
type t = {
  edges: Edge.t array;
  vertices: Types.vertex array;
  facets: Facet.t array;
}

(** Convert vertices and vertex indexed faces. Given original data must necessary
    convex mesh, and a front face of given faces is counter-clockwise which vertices consisted of
    face is.
*)
val convert: vertices:Types.vertex array -> faces:Facet.vertex_ids array -> t
