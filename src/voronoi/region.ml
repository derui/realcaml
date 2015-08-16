open Core.Std

module A = Typedvec.Std.Algebra.Vec
module V = A.Vec
module S = Typedvec.Std.Size
module UV = Realcaml_util.Vec

type v = S.three A.vec

module Edge_region = struct
  (* TRANSLATE: エッジの始点と終点、エッジの法線ベクトル、面の法線ベクトル *)
  type t = {
    edge: v * v;
    normal: v;
    face_normal: v;
  }

  let make ~edge ~normal ~face_normal = {
    edge; normal; face_normal
  }
end

module Face_region = struct
  (* TRANSLATE: 基準となる点と、点から伸びるエッジそれぞれの法線ベクトル、面の法線ベクトル *)
  type t = {
    base: v;
    a_normal: v;
    b_normal: v;
    normal: v;
  }

  let make ~base ~a ~b ~normal = {
    base; a_normal = a; b_normal = b; normal;
  }
end

type region_type = [`Edge of Edge_region.t | `Point of Point_region.t]

type t = region_type list

(* TRANSLATE : エッジと面の法線から、エッジのボロノイ領域を取得する *)
let make_edge_region (v1, v2) normal =
  let open V.Open in
  let edge_normal = V.cross (v2 -: v1) normal |> V.normalize in
  Edge_region.make ~edge:(v1, v2) ~normal:edge_normal ~face_normal:normal

(* TRANSLATE: 指定された点を含む二本のエッジを取得する *)
let point_of_contained_edge v (e1, e2, e3) =
  let point_contained (e1, e2) v = V.compare v e1 = 0 ||  V.compare v e2 = 0 in
  let cont_e1 = point_contained e1 v
  and cont_e2 = point_contained e2 v
  and cont_e3 = point_contained e3 v in
  match (cont_e1, cont_e2, cont_e3) with
  | (true, true, _) -> (e1, e2)
  | (_, true, true) -> (e2, e3)
  | (true, _, true) -> (e3, e1)
  | _ -> failwith "matching point of edge not found"

(* TRANSLATE: 点のボロノイ領域を求める *)
let make_point_region (e1, e2, e3) normal point =
  let e1, e2 = point_of_contained_edge point (e1, e2, e3) in
  match (make_edge_region e1 normal, make_edge_region e2 normal) with
  | (REdge ((e11, e12), normal1, _), REdge ((e21, e22), normal2, _)) ->
     RPoint (point, V.sub e12 e11 |> V.normalize, V.sub e21 e22 |> V.normalize, normal)
  | _ -> failwith "error"

let make_region (v1, v2, v3) =
  let edges = [(v1, v2); (v2, v3); (v3, v1)] in
  let e1, e2 = (V.sub v2 v1, V.sub v3 v2) in
  let normal = V.cross e1 e2 |> V.normalize in

  let open Sugarpot.Std.Prelude in
  let edge_regions = List.map (flip make_edge_region normal) edges in
  let edges = ((v1, v2), (v2, v3), (v3, v1)) in
  let point_regions = List.map (make_point_region edges normal) [v1;v2;v3] in
  List.concat [edge_regions; point_regions]
