(**
   Providing facet type of mesh.

   @version 0.1
   @author derui
*)

type vertex_ids = int * int * int
(* The type of vertex ids contains the Facet. *)
type edge_id = Realcaml_mesh_types.edge_id
type edge_ids = edge_id * edge_id * edge_id
(* The type of edge ids contains the Facet. *)

type normal = Realcaml_util.vec
type edge = Realcaml_mesh_types.vertex * Realcaml_mesh_types.vertex
type facet = (Realcaml_mesh_edge.vertex_ids * Realcaml_mesh_edge.vertex_ids * Realcaml_mesh_edge.vertex_ids)

type t = {
  facet_id: Realcaml_mesh_types.facet_id;
  (* The identity of this *)
  vertex_ids: vertex_ids;
  (* vertex ids including this facet. *)
  edge_ids: edge_ids;
  (* edge ids construcing this facet. *)
  normal: normal;
  (* The normal vector of this facet. *)
}


(* TRANSLATE: a,b,cがなす三角形のedgeを生成する

   生成されるエッジは、それぞれ ∠C、∠A、∠B の対面となるエッジとなる
*)
val edges_of_face: vertex_ids -> facet

val alloc_facet_id: vertex_ids array -> (Realcaml_mesh_types.facet_id * vertex_ids) array

(* TRANSLATE: 渡されたエッジが成す平面が、縮退面であるかどうかを判別する *)
val is_degenerate: a:edge -> b:edge -> c:edge -> unit -> bool
