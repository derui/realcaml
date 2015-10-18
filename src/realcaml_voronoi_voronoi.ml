open Core.Std

module Region = Realcaml_voronoi_region
module V = Typedvec.Std.Algebra.Vec
module F = Realcaml_mesh.Facet
module E = Realcaml_mesh.Edge

type t = Region.t list

(* TRANSLATE: ある点を、法線ベクトルの面上に投影する *)
let projection_point ~base ~normal ~point =
  let open V.Open in
  let subbed = normal *: (point -: base) in
  let multiplied = V.scalar ~scale:subbed normal in
  V.sub point multiplied

let recent_of_region ~point region =
  let is_contain_shape =
    List.map ~f:Region.to_region_type region |> List.for_all ~f:(function
        | `Edge({Region.Edge_region.Region.edge = (e1, e2); normal = enormal; face_normal = snormal}) -> 
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
        | `Edge ({Region.Edge_region.Region.edge = (e1, e2); normal = enormal; face_normal = snormal}) -> 
          projection_point ~base:e1 ~normal:snormal ~point
        | _ -> failwith "can not calculate projection point on the plane"
      )
    |> List.hd_exn
  in

  if is_contain_shape then point_in_shape
  else
    let types = List.map region ~f:Region.to_region_type in
    let calculated = List.map types ~f:(function
      | `Edge t -> Region.Edge_region.Region.recent_point ~point:point_in_shape t
      | `Point t -> Region.Point_region.Region.recent_point ~point:point_in_shape t
    ) |> List.filter ~f:(Option.is_some) in
    match calculated with
    | x :: _ -> Option.value_exn x
    | _ -> failwith "recent region not found..."

let voronoi_region mesh facet =
  (* TRANSLATE: 三角形におけるボロノイ領域を求める *)
  let vertices = mesh.Realcaml_mesh.Mesh.vertices in
  let facet = mesh.Realcaml_mesh.Mesh.facets |> Fn.flip Array.get facet in
  let v1, v2, v3 = facet.F.vertex_ids in

  (* TRANSLATE: 三角形のボロノイ領域になる6個のボロノイ領域を返却する *)
  Region.make (vertices.(v1), vertices.(v2), vertices.(v3))

