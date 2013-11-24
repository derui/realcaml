open Sugarpot.Std.Prelude

module V = Candyvec.Vector
module F = Mesh.Facet
module E = Mesh.Edge

type region_type =

(* TRANSLATE: エッジの始点と終点、エッジの法線ベクトル、面の法線ベクトル *)
| REdge of (V.t * V.t) * V.t * V.t
(* TRANSLATE: 基準となる点と、点から伸びるエッジそれぞれの法線ベクトル、面の法線ベクトル *)
| RPoint of V.t * V.t * V.t * V.t

type t = region_type list

(* TRANSLATE: Pointは最近接点が、いずれかの頂点。
   Edgeは、最近接点が、いずれかのエッジ上に存在する。
   Shapeは、平面上に最近接点が存在する。
   それぞれ最近接点が含まれる
*)
type recent_type = Point of V.t
                   | Edge of V.t * V.t * V.t
                   | Shape of V.t

module Base = struct
  type t = region_type
  type triangle = V.t * V.t * V.t

  (* TRANSLATE : エッジと面の法線から、エッジのボロノイ領域を取得する *)
  let make_edge_region (v1, v2) normal =
    let edge_normal = V.cross (V.sub v2 v1) normal |> V.normalize in
    REdge ((v1, v2), edge_normal, normal)

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

  (* TRANSLATE: ある点を、法線ベクトルの面上に投影する *)
  let projection_point ~base ~normal ~point =
    let subbed = V.dot normal (V.sub point base) in
    let multiplied = V.scale ~scale:subbed ~v:normal in
    V.sub point multiplied

  let is_contain edge point = V.dot point edge >= 0.0

  (* translate: エッジのボロノイ領域に含まれるかどうかを返却する *)
  let contain_region_for_edge edge point =
    match edge with
    | (REdge((e1, e2), enormal, _)) -> 
      let open Sugarpot.Std.List in
      let edge1 = V.normalize (V.sub e2 e1) in
      let edge2 = V.invert edge1
      and base1 = V.sub point e1
      and base2 = V.sub point e2 in
      List.for_all id
        [is_contain edge1 base1; is_contain edge2 base2; is_contain enormal point]
    | _ -> failwith "contain_region_for_edge can only apply to REdge"

  let contain_region_for_point p point =
    match p with
    | RPoint (base, e1, e2, normal) ->
      let base = V.sub point base in
      let is_contain edge = is_contain edge base in
      is_contain e1 && is_contain e2
    | _ -> failwith "contain_region_for_point can only apply to RPoint"
  
  let recent_region ~region ~point =
    let is_contain_shape =
      List.for_all (function
      | REdge ((e1, e2), enormal, snormal) -> 
        let projected = projection_point ~base:e1 ~normal:snormal ~point in
        (V.dot projected enormal) < 0.0
      | _ -> true
      ) region
    in
    let get_point_in_shape = 
      List.hd |< List.map (function
      | REdge ((e1, e2), enormal, snormal) ->
        projection_point ~base:e1 ~normal:snormal ~point
      | _ -> failwith "can not calculate projection point on the plane"
      ) (List.filter (function | REdge _ -> true | _ -> false) region)
    in

    if is_contain_shape then Shape get_point_in_shape
    else
      let calculated = List.filter (fun current ->
        match current with
        | REdge _ -> contain_region_for_edge current get_point_in_shape
        | RPoint _ -> contain_region_for_point current get_point_in_shape
      ) region in
      match calculated with
      | x :: _ ->
        begin match x with
        | REdge ((e1, e2), enormal, snormal) -> 
          let edge = V.sub e2 e1 in
          let recent_point = V.scale ~v:edge ~scale:(V.dot edge (V.sub point e1)) in
          Edge (recent_point, e1, e2)
        | RPoint (p, _, _, _) -> Point p
        end
      | _ -> failwith "recent region not found..."
end

let voronoi_region mesh facet =
  (* TRANSLATE: 三角形におけるボロノイ領域を求める *)
  let vertices = mesh.Mesh.vertices in
  let facet = mesh.Mesh.facets |> flip Array.get facet in
  let v1, v2, v3 = facet.F.vertex_ids in

  (* TRANSLATE: 三角形のボロノイ領域になる6個のボロノイ領域を返却する *)
  Base.make_region (vertices.(v1), vertices.(v2), vertices.(v3))

(* TRANSLATE: 対象が、あるボロノイ領域における最近接領域の種類を取得する *)
let recent_of_region target voronois =
  Base.recent_region ~region:voronois ~point:target

(* Get closest point from given voronoi regions with target point.  *)
let expand_recent_point = function
  | Point(point) -> point
  | Edge(nearest, _, _) -> nearest
  | Shape p -> p
