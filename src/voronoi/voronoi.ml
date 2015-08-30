open Core.Std

module V = Typedvec.Std.Algebra.Vec
module F = Realcaml_mesh.Facet
module E = Realcaml_mesh.Edge

type t = Region.t list

(* TRANSLATE: ある点を、法線ベクトルの面上に投影する *)
let projection_point ~base ~normal ~point =
  let open V.Open in
  let subbed = normal *: (point -: base) in
  let multiplied = V.scalar ~scale:subbed ~v:normal in
  V.sub point multiplied

let is_contain edge point = V.dot point edge >= 0.0

(* translate: エッジのボロノイ領域に含まれるかどうかを返却する *)
let contain_region_for_edge edge point =
  let {Region.Edge_region.edge = (e1, e2);
       Region.Edge_region.normal = enormal;
       _} = edge in
  let open V.Open in
  let edge1 = V.normalize (e2 -: e1) in
  let edge2 = V.inverse edge1
  and base1 = point -: e1
  and base2 = point -: e2 in
  List.for_all ~f:ident
    [is_contain edge1 base1; is_contain edge2 base2; is_contain enormal point]

let contain_region_for_point p point =
  let open V.Open in
  let {Region.Point_region.base = base;
       a_normal = e1;
       b_normal = e2;
       normal} = p in
  
  let base = point -: base in
  let is_contain edge = is_contain edge base in
  is_contain e1 && is_contain e2
    
let recent_of_region ~point region =
  let is_contain_shape =
    List.map ~f:Region.to_region_type region |> List.for_all ~f:(function
    | `Edge({Region.Edge_region.edge = (e1, e2); normal = enormal; face_normal = snormal}) -> 
       let projected = projection_point ~base:e1 ~normal:snormal ~point in
       (V.dot projected enormal) < 0.0
    | _ -> true
    )
  in
  let point_in_shape = 
    List.map region ~f:Region.to_region_type |> List.filter ~f:(function
    | `Edge _ -> true
    | _ -> false
    )
      |> List.map ~f:(function
        | `Edge ({Region.Edge_region.edge = (e1, e2); normal = enormal; face_normal = snormal}) -> 
           projection_point ~base:e1 ~normal:snormal ~point
        | _ -> failwith "can not calculate projection point on the plane"
      )
      |> List.hd_exn
  in

  if is_contain_shape then point_in_shape
  else
    let types = List.map region ~f:Region.to_region_type in
    let calculated = List.filter types ~f:(function
      | `Edge t -> contain_region_for_edge t point_in_shape
      | `Point t -> contain_region_for_point t point_in_shape
    ) in
    match calculated with
    | x :: _ ->
       begin match x with
       | `Edge {Region.Edge_region.edge =(e1, e2);normal= enormal; face_normal = snormal} -> 
          let open V.Open in
          let edge = e2 -: e1 in
          let recent_point = V.scalar ~v:edge ~scale:(edge *: (point -: e1)) in
          recent_point
       | `Point {Region.Point_region.base = p; _} -> p
       end
    | _ -> failwith "recent region not found..."

let voronoi_region mesh facet =
  (* TRANSLATE: 三角形におけるボロノイ領域を求める *)
  let vertices = mesh.Realcaml_mesh.Mesh.vertices in
  let facet = mesh.Realcaml_mesh.Mesh.facets |> Fn.flip Array.get facet in
  let v1, v2, v3 = facet.F.vertex_ids in

  (* TRANSLATE: 三角形のボロノイ領域になる6個のボロノイ領域を返却する *)
  Region.make (vertices.(v1), vertices.(v2), vertices.(v3))

