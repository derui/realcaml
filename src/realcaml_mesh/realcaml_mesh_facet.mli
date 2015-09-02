(**
   Providing facet type of mesh.

   @version 0.1
   @author derui
*)

module U = Realcaml_util
module Types = Realcaml_mesh_types
module Edge = Realcaml_mesh_edge

type vertex_ids = int * int * int
(* The type of vertex ids contains the Facet. *)
type edge_ids = Types.edge_id * Types.edge_id * Types.edge_id
(* The type of edge ids contains the Facet. *)

type normal = U.vec

type t = {
  facet_id: Types.facet_id;
  (* The identity of this *)
  vertex_ids: vertex_ids;
  (* vertex ids including this facet. *)
  edge_ids: edge_ids;
  (* edge ids construcing this facet. *)
  normal: normal;
  (* The normal vector of this facet. *)
}

type edge = Types.vertex * Types.vertex

(* TRANSLATE: a,b,cがなす三角形のedgeを生成する

   生成されるエッジは、それぞれ ∠C、∠A、∠B の対面となるエッジとなる
*)
val edges_of_face: vertex_ids -> (Edge.vertex_ids * Edge.vertex_ids * Edge.vertex_ids)

val alloc_facet_id: vertex_ids array -> (Types.facet_id * vertex_ids) array

(* TRANSLATE: 渡されたエッジが成す平面が、縮退面であるかどうかを判別する *)
val is_degenerate: a:edge -> b:edge -> c:edge -> unit -> bool
