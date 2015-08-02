open Core.Std

type vertex_ids = int * int * int
type edge_ids = Types.edge_id * Types.edge_id * Types.edge_id

module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size
type normal = S.three S.t A.vec

type t = {
  facet_id: Types.facet_id;
  vertex_ids: vertex_ids;
  edge_ids: edge_ids;
  normal: normal;
}

let empty = {
  facet_id = -1;
  vertex_ids = (0,0,0);
  edge_ids = (0,0,0);
  normal = A.Vec.make S.three 0.0;
}

type edge = Types.vertex * Types.vertex

let edges_of_face (a, b, c) = ((a, b), (b, c), (c, a))

let alloc_facet_id faces =
  let ids = List.range ~start:`inclusive ~stop:`exclusive 0 (Array.length faces) in
  Array.to_list faces |> List.zip_exn ids |> Array.of_list

let is_degenerate ~a ~b ~c () =
  let module V = Typedvec.Std.Algebra.Vec in
  let open V.Open in
  (* Get edge vectors each of edge vertices.  *)
  let edge_a = V.normalize ((fst a) -: (snd a))
  and edge_b = V.normalize ((fst b) -: (snd b))
  and edge_c = V.normalize ((fst c) -: (snd c)) in
  if Float.abs (edge_a *: (V.inverse edge_c)) = 1.0 then true
  else if Float.abs (edge_b *: (V.inverse edge_a)) = 1.0 then true
  else if Float.abs (edge_c *: (V.inverse edge_b)) = 1.0 then true
  else false
