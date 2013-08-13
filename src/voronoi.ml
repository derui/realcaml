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
*)
type recent_type = Point of V.t
                   | Edge of V.t * V.t
                   | Shape

module Base = struct
  type t = region_type
  type triangle = V.t * V.t * V.t

  (* TRANSLATE : エッジと面の法線から、エッジのボロノイ領域を取得する *)
  let make_edge_region (v1, v2) normal =
    let edge_normal = V.cross normal (V.sub v2 v1) in
    REdge ((v1, v2), edge_normal, normal)
  ;;

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
  ;;

  (* TRANSLATE: 点のボロノイ領域を求める *)
  let make_point_region (e1, e2, e3) point =
    let e1, e2 = point_of_contained_edge point (e1, e2, e3) in
    match (make_edge_region e1, make_edge_region e2) with
    | (REdge (_, normal1, _), REdge (_, normal2, _)) ->
      RPoint (point, normal1, normal2, normal)
    | _ -> failwith "error"
  ;;

  let make_region (v1, v2, v3) =
    let edges = [(v1, v2); (v2, v3); (v3, v1)] in
    let e1, e2 = (V.sub v2 v1, V.sub v3 v2) in
    let normal = V.cross e1 e2 in

    let open Sugarpot.Std.Prelude in
    let edge_regions = List.map (flip make_edge_region normal) edges in
    let edges = ((v1, v2), (v2, v3), (v3, v1)) in
    let point_regions = List.map (make_point_region edges) [v1;v2;v3] in
    List.concat [edge_regions; point_regions]
  ;;


  let recent_region ~region ~point = failwith "not implements yet"
end

let voronoi_region mesh facet =
  (* TRANSLATE: 三角形におけるボロノイ領域を求める *)
  let vertices = mesh.Mesh.vertices in
  let facet = mesh.Mesh.facets |> flip Array.get facet in
  let edges = mesh.Mesh.edges in
  let v1, v2, v3 = facet.F.vertex_ids 
  and e1, e2, e3 = facet.F.edge_ids
  and normal = facet.F.normal in

  (* TRANSLATE: 三角形のボロノイ領域になる6個のボロノイ領域を返却する *)
  Base.make_region (vertices.(v1), vertices.(v2), vertices.(v3))
;;

(* TRANSLATE: ある点を、法線ベクトルの面上に投影する *)
let projection_point target normal vec =
  let dotted = V.dot (V.sub target vec) normal in
  V.sub target (V.scale ~scale:dotted ~v:normal)

(* TRANSLATE: 対象が、あるボロノイ領域における最近接領域の種類を取得する *)
let recent_of_region target voronois =
  let point, normal = match voronois with
    | [] -> failwith "voronoi regions has to have one or more region at least"
    | RPoint (v,_, _, normal) :: _ -> (v, normal)
    | REdge ((v, _), _, normal) :: _ -> (v, normal) in
  (* TRANSLATE: ここで、shapeの面に点を投影する *)
  let project = projection_point target  normal point in
  let voronoi_edge = List.filter (function REdge _ -> true | _ -> false) voronois in

  (* TRANSLATE: 投影点について、それぞれのエッジの法線について、表側なのか裏側なのかを判定する *)
  let is_front normal base =
    let base_point = V.sub project base in
    let dot = V.dot mbase_point normal in
    dot >= 0.0 in
  let is_back normal base = not |< is_front normal base in

  (* TRANSLATE: 各エッジの面について裏側に位置する場合、最近接点はその点となる *)
  let is_contain_shape =
    List.for_all (function
    | REdge ((b1, b2), enormal, normal) -> is_back target enormal b1
    | _ -> failwith "is_contain_shape do not have not REdge one"
    ) voronoi_edge in
  if is_contain_shape then Shape
  else
    (* TRANSLATE: 面ではない場合、エッジか頂点が最近接点となるため、それぞれのボロノイ領域において、すべての面に
       対して表側に位置するボロノイ領域が、最近接点を含む領域となる
    *)
    let recent = List.filter (fun x ->
      match x with
      | REdge ((ebegin, eend), enor, nor) ->
        let side_nor1 = V.normalize (V.sub eend ebegin) in
        let side_nor2 = V.invert side_nor1 in
        (is_front side_nor1 ebegin) &&
          (is_front side_nor2 eend)

      | RPoint (point, enor1, enor2, normal) ->
        let side_nor1 = V.normalize (V.cross enor1 normal)
        and side_nor2 = V.normalize (V.cross normal enor2) in
        (is_front target side_nor1 point) && (is_front target side_nor2 point)
    ) voronois in
    match recent with
    | [] -> failwith "what point is contained in the voronoi region have to be one or more region"
    | REdge ((ebegin, eend), _, _) :: _ -> Edge (ebegin, eend)
    | RPoint (point, _, _, _) :: _ -> Point point
