open Baselib.Std.Prelude

module V = Vecmath.Vector
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

let voronoi_region mesh facet =
  (* TRANSLATE: 三角形におけるボロノイ領域を求める *)
  let vertices = mesh.Mesh.vertices in
  let facet = mesh.Mesh.facets |> flip Array.get facet in
  let edges = mesh.Mesh.edges in
  let v1, v2, v3 = facet.F.vertex_ids 
  and e1, e2, e3 = facet.F.edge_ids
  and normal = facet.F.normal in
  (* TRANSLATE: エッジにおけるボロノイ領域を求める *)
  let region_of_edge edge =
    let v1, v2 = edge.E.vertex_ids in
    let v1, v2 = (vertices.(v1), vertices.(v2)) in
    let vedge1 = V.cross v1 normal in
    REdge ((v1, v2), vedge1, normal) in

  (* TRANSLATE: 指定された点を含む二本のエッジを取得する *)
  let point_of_contained_edge v (e1, e2, e3) =
    let edges = mesh.Mesh.edges in
    let e1 = edges.(e1)
    and e2 = edges.(e2)
    and e3 = edges.(e3) in
    let point_contained e v = let ev1, ev2 = e.E.vertex_ids in
                              v == ev1 || v == ev2 in
    let cont_e1 = point_contained e1 v
    and cont_e2 = point_contained e2 v
    and cont_e3 = point_contained e3 v in
    if cont_e1 && cont_e2 then (e1, e2)
    else if cont_e2 && cont_e3 then (e2, e3)
    else (e3, e1) in

  (* TRANSLATE: 点のボロノイ領域を求める *)
  let region_of_point point =
    let e1, e2 = point_of_contained_edge point facet.F.edge_ids in
    match (region_of_edge e1, region_of_edge e2) with
    | (REdge (_, normal1, _), REdge (_, normal2, _)) ->
      RPoint (vertices.(point), normal1, normal2, normal)
    | _ -> failwith "error" in

  (* TRANSLATE: 三角形のボロノイ領域になる6個のボロノイ領域を返却する *)
  [ region_of_edge edges.(e1); region_of_edge edges.(e2); region_of_edge edges.(e3);
    region_of_point v1; region_of_point v2; region_of_point v3]

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
  let project = projection_point target  normal point in
  let voronoi_edge = List.filter (function REdge _ -> true | _ -> false) voronois in

  let is_front point normal base =
    let base_point = V.add project (V.invert base) in
    let dot = V.dot base_point normal in
    dot >= 0.0 in
  let is_back point normal base = not |< is_front point normal base in

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
        (is_front target side_nor1 ebegin) &&
          (is_front target side_nor2 eend)

      | RPoint (point, enor1, enor2, normal) ->
        let side_nor1 = V.normalize (V.cross enor1 normal)
        and side_nor2 = V.normalize (V.cross normal enor2) in
        (is_front target side_nor1 point) && (is_front target side_nor2 point)
    ) voronois in
    match recent with
    | [] -> failwith "what point is contained of voronoi region have to be one or more region"
    | REdge ((ebegin, eend), _, _) :: _ -> Edge (ebegin, eend)
    | RPoint (point, _, _, _) :: _ -> Point point
