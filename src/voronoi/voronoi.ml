open Core
module V = Typedvec.Algebra.Vec
module F = Realcaml_mesh.Facet
module E = Realcaml_mesh.Edge

type t = Region.t list

let recent_of_region ~point region =
  let types = List.map region ~f:Region.to_region_type in
  let calculated =
    List.map types ~f:(function
      | `Edge t  -> Edge_region.Region.recent_point ~point t
      | `Point t -> Point_region.Region.recent_point ~point t
      | `Plane t -> Plane_region.Region.recent_point ~point t)
    |> List.filter ~f:Option.is_some
  in
  match calculated with x :: _ -> Option.value_exn x | _ -> failwith "recent region not found..."

let voronoi_region mesh facet =
  (* TRANSLATE: 三角形におけるボロノイ領域を求める *)
  let vertices = mesh.Realcaml_mesh.Mesh.vertices in
  let facet = mesh.Realcaml_mesh.Mesh.facets |> Fn.flip Array.get facet in
  let v1, v2, v3 = facet.F.vertex_ids in

  (* TRANSLATE: 三角形のボロノイ領域になる6個のボロノイ領域を返却する *)
  Region.make (vertices.(v1), vertices.(v2), vertices.(v3))
