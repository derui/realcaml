open Core
module V = Typedvec.Algebra.Vec

module Tuple_key = struct
  type t = int * int

  let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp

  let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t

  let compare = Stdlib.compare
end

module M = Map.Make (Tuple_key)

type t = {
  edges : Edge.t array;
  vertices : Types.vertex array;
  facets : Facet.t array;
  edge_facet_map : Edge_facet_map.t;
}
(** type of convex mesh *)

let vertex_of_edge vertices (ai, bi) = (vertices.(ai), vertices.(bi))

(* faceで利用されているエッジを、ユニークなVertexの組み合わせとみなした結果の配列を返す。 *)
let unique_edges faces = Array.concat_map faces ~f:(fun (a, b, c) -> [| a; b; c |])

let find_edge edges vert = match M.find edges vert with None -> M.find edges (Tuple2.swap vert) | _ as e -> e

(* IDが付与されて生成されたエッジをマップにする *)
let make_edge_map edges =
  Array.fold edges ~init:M.empty ~f:(fun m edge ->
      let vert = edge.Edge.vertex_ids in
      match find_edge m vert with None -> M.set m ~key:edge.Edge.vertex_ids ~data:edge | _ -> m)

let convert ~vertices ~faces () =
  (* TODO 縮退面を事前に削除する。 *)
  let faces =
    Array.filter faces ~f:(fun e ->
        let ea, eb, ec = Facet.edges_of_face e in
        let a = vertex_of_edge vertices ea and b = vertex_of_edge vertices eb and c = vertex_of_edge vertices ec in
        not (Facet.is_degenerate ~a ~b ~c ()))
  in
  let faces = Facet.alloc_facet_id faces in
  let edges = Array.map faces ~f:(fun face -> snd face |> Facet.edges_of_face) |> unique_edges in
  let edges = Edge.alloc_edge_id edges |> make_edge_map in

  let faces =
    Array.map faces ~f:(fun (index, face) ->
        let ea, eb, ec = Facet.edges_of_face face in

        let ea = find_edge edges ea and eb = find_edge edges eb and ec = find_edge edges ec in
        match (ea, eb, ec) with
        | None, _, _ | _, None, _ | _, _, None -> failwith "not found some edge of face"
        | Some ea, Some eb, Some ec            ->
            let va, vb, vc = face in
            let open V.Open in
            let normal =
              V.cross ~left:(vertices.(vb) -: vertices.(va)) ~right:(vertices.(vb) -: vertices.(vc)) |> V.normalize
            in
            let open Edge in
            {
              Facet.facet_id = index;
              vertex_ids = (va, vb, vc);
              edge_ids = (ea.edge_id, eb.edge_id, ec.edge_id);
              normal;
            })
  in
  let edges = M.data edges |> Array.of_list in
  { vertices; edges; facets = faces; edge_facet_map = Edge_facet_map.make ~edges ~facets:faces () }
