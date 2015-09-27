(**
   Providing edge type of mesh.

   @version 0.1
   @author derui
*)

open Core.Std

module Types = Realcaml_mesh_types

type edge_type = Convex | Concave | Flat

type vertex_ids = int * int

type t = {
  edge_id: Types.edge_id;
  edge_type: edge_type;
  vertex_ids: vertex_ids;
}

let alloc_edge_id edges =
  let ids = List.range ~start:`inclusive ~stop:`exclusive 0 (Array.length edges) in
  Array.to_list edges |> List.zip_exn ids |> List.map ~f:(fun (edge_id, vertex_ids) ->
      {edge_type = Convex; edge_id; vertex_ids}
    ) |> Array.of_list
