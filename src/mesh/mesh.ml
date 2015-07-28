module V = Typedvec.Algebra.Vec

open Core.Std

module Tuple_key = struct
  type t = int * int
  let t_of_sexp = failwith "not implemented"
  let sexp_of_t = failwith "not implemented"
  let compare a b =
    let comp a b = 
      let first = Pervasives.compare (fst a) (fst b) in
      if first = 0 then Pervasives.compare (snd a) (snd b) else first
    in
    let default = comp a b
    and swapped = comp a (Tuple2.swap b) in
    if default = 0 then default else swapped
end

module S = Set.Make(Tuple_key)
module M = Map.Make(Tuple_key)

let max_vertices = 34
let max_edges = 96
let max_facets = 64

(** type of convex mesh  *)
type t = {
  edges: Edge.t array;
  vertices: Types.vertex array;
  facets: Facet.t array;
}

let vertex_of_edge vertices (ai, bi) = (vertices.(ai), vertices.(bi))

(* faceで利用されているエッジを、ユニークなVertexの組み合わせとみなした結果の配列を返す。 *)
let unique_edges faces =
  let set = S.empty in
  let set = Array.fold faces ~init:set ~f:(fun set (a, b, c) ->
    let s = S.add set a in
    let s = S.add s b in
    S.add s c
  ) in
  S.to_array set

(* IDが付与されて生成されたエッジをマップにする *)
let make_edge_map edges =
  Array.fold edges ~init:M.empty ~f:(fun map edge ->
    M.add map ~key:edge.Edge.vertex_ids ~data:edge
  )

let convert ~vertices ~faces =
  (* TODO 縮退面を事前に削除する。 *)
  let faces = Array.filter faces ~f:(fun e ->
    let (ea, eb, ec) = Facet.edges_of_face e in
    let a = vertex_of_edge vertices ea
    and b = vertex_of_edge vertices eb
    and c = vertex_of_edge vertices ec in
    not (Facet.is_degenerate ~a ~b ~c ())) in
  let faces = Facet.alloc_facet_id faces in
  let edges = Array.map faces ~f:(fun face -> snd face |> Facet.edges_of_face) |> unique_edges in
  let edges = Edge.alloc_edge_id edges |> make_edge_map in

  let faces = Array.map faces ~f:(fun (index, face) ->
    let (ea, eb, ec) = Facet.edges_of_face face in

    let ea = M.find edges ea
    and eb = M.find edges eb
    and ec = M.find edges ec in
    match (ea, eb, ec) with
    | (None,_,_) | (_,None,_) | (_,_,None) -> failwith "not found some edge of face"
    | (Some ea, Some eb, Some ec) ->
       let va, vb, vc = face in
       let open V.Open in
       let normal = V.cross (vertices.(vb) -: vertices.(va)) (vertices.(vb) -: vertices.(vc)) |> V.normalize in
       let open Edge in
       {Facet.facet_id = index;
        vertex_ids = (va, vb, vc);
        edge_ids = (ea.edge_id, eb.edge_id, ec.edge_id);
        normal}
  ) in
  {vertices = vertices;
   edges = M.to_alist edges |> List.map ~f:snd |> Array.of_list;
   facets = faces}
