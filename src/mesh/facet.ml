open Core
module U = Realcaml_util
module A = Typedvec.Algebra
module S = Typedvec.Size

type vertex_ids = int * int * int

type edge_id = Types.edge_id

type edge_ids = edge_id * edge_id * edge_id

type normal = U.Types.vec

type edge = Types.vertex * Types.vertex

type facet = Edge.vertex_ids * Edge.vertex_ids * Edge.vertex_ids

type t = {
  facet_id : Types.facet_id;
  vertex_ids : vertex_ids;
  edge_ids : edge_ids;
  normal : normal;
}

let edges_of_face (a, b, c) = ((a, b), (b, c), (c, a))

let alloc_facet_id faces =
  let ids = List.range ~start:`inclusive ~stop:`exclusive 0 (Array.length faces) in
  Array.to_list faces |> List.zip_exn ids |> Array.of_list

let is_degenerate ~a ~b ~c () =
  let module V = Typedvec.Algebra.Vec in
  let open V.Open in
  (* Get edge vectors each of edge vertices. *)
  let edge_a = V.normalize (fst a -: snd a)
  and edge_b = V.normalize (fst b -: snd b)
  and edge_c = V.normalize (fst c -: snd c) in
  let open Float in
  if Float.abs (edge_a *: V.inverse edge_c) = 1.0 then true
  else if Float.abs (edge_b *: V.inverse edge_a) = 1.0 then true
  else if Float.abs (edge_c *: V.inverse edge_b) = 1.0 then true
  else false
