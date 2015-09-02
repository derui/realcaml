(**
   Providing mesh data and operations to detect collision. Provided mesh data is always convex mesh.

   @version 0.1
   @author derui
*)

module Edge = Realcaml_mesh_edge
module Facet = Realcaml_mesh_facet
module Types = Realcaml_mesh_types
module Edge_facet_map = Realcaml_mesh_edge_facet_map

(** type of convex mesh  *)
type t = {
  edges: Edge.t array;
  vertices: Types.vertex array;
  facets: Facet.t array;
  edge_facet_map: Edge_facet_map.t
}

(** Convert vertices and vertex indexed faces. Given original data must necessary
    convex mesh, and a front face of given faces is counter-clockwise which vertices consisted of
    face is.
*)
val convert: vertices:Types.vertex array -> faces:Facet.vertex_ids array -> t
