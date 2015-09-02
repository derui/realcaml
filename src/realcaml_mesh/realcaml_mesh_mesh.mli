(**
   Providing mesh data and operations to detect collision. Provided mesh data is always convex mesh.

   @version 0.1
   @author derui
*)

(** type of convex mesh  *)
type t = {
  edges: Realcaml_mesh_edge.t array;
  vertices: Realcaml_mesh_types.vertex array;
  facets: Realcaml_mesh_facet.t array;
  edge_facet_map: Realcaml_mesh_edge_facet_map.t
}

(** Convert vertices and vertex indexed faces. Given original data must necessary
    convex mesh, and a front face of given faces is counter-clockwise which vertices consisted of
    face is.
*)
val convert: vertices:Realcaml_mesh_types.vertex array ->
  faces:Realcaml_mesh_facet.vertex_ids array -> t
