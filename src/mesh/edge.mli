(** Providing edge type of mesh.

    @version 0.1
    @author derui *)

(* Variation of edge. *)
type edge_type =
  | Convex
  | Concave
  | Flat

(* Vertex ids are included by this edge. *)
type vertex_ids = int * int

type t = {
  edge_id : Types.edge_id;
  (* The identity of edge *)
  edge_type : edge_type;
  (* The type of this edge. *)
  vertex_ids : vertex_ids; (* vertex ids that are included this edge. *)
}
(** A type of Edge *)

val alloc_edge_id : vertex_ids array -> t array
(* [alloc_edge_id edges] get to be allocated id for each edge. Notice, an edge that is gotten this function is only as
   `convex' edge. *)
