open Core.Std

module A = Typedvec.Std.Algebra
module V = A.Vec
module S = Typedvec.Std.Size
module U = Realcaml_util

module Edge_region = Realcaml_voronoi_edge_region
module Point_region = Realcaml_voronoi_point_region

type v = U.vec

type region_type = [`Edge of Edge_region.Region.t | `Point of Point_region.Region.t]

type t = region_type

type triangle = v * v * v
(* The type of triangle. *)

let make (v1, v2, v3) =
  let edges = [(v1, v2); (v2, v3); (v3, v1)] in
  let e1, e2 = (V.sub v2 v1, V.sub v3 v2) in
  let normal = V.cross e1 e2 |> V.normalize in

  let edge_regions = List.map edges ~f:(Fn.flip Edge_region.make normal) in
  let edge_regions = List.map edge_regions ~f:(fun e -> `Edge e) in
  let edges = ((v1, v2), (v2, v3), (v3, v1)) in
  let point_regions = List.map [v1;v2;v3] ~f:(Point_region.make edges normal)  in
  let point_regions = List.map point_regions ~f:(fun p -> `Point p)  in
  List.concat [edge_regions; point_regions]

let to_region_type = ident
