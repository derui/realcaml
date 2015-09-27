open Core.Std

module Voronoi = Realcaml_voronoi.Voronoi
module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size
module V = A.Vec
module M = A.Mat
module Mesh = Realcaml_mesh
module U = Realcaml_util

(* Result of functions in this module. That types are each positions to first and second mesh,
   and last float is distination between them.
*)
type t = {
  normal : U.vec;
  point_a : U.vec;
  point_b : U.vec;
  depth : float;
}

let sort_points list = List.sort ~cmp:(fun {depth = d1;_} {depth = d2;_} -> compare d1 d2) list

(* Check the plane to be equal direction for normal. *)
let is_observe_face plane normal =
  let open V.Open in plane.Mesh.Facet.normal *: normal >= 0.0

let convert_vertices mat mesh = 
  let module MS = Mesh.Mesh in
  let vertices = mesh.MS.vertices in
  let open A.Open in
  Array.map vertices ~f:(fun v -> (U.Vec.to_four v) *> mat |> U.Vec.to_three)

(* TODO: 最近接点を探す際、どちらか一方のどれかの形状を座標系として
   利用できるようにする。bodyが入れ替わっても、座標系は変わってはならない。
   最終的に取得した点はワールド座標系における衝突点として、それぞれの
   ローカル座標における衝突点を最終的に必要とする。ここでは、逆行列を必ず求める必要が
   あるので、簡便のために、正当ではない逆行列を返す可能性があるような関数を4x4行列に
   作っておく。
*)
let get_plane_closest_points (axis, _) body_a body_b trans_mat =
  let open Realcaml_rigid_body in
  let shapes = body_a.Rigid_body_info.collidable.Collidable.shapes in
  let world_b =
    let open M.Open in
    let world_a = Rigid_body_info.get_world_transform body_a in
    let inverted = match M.inverse world_a with
      | Some m -> m
      | None -> M.identity S.four
    in
    (Rigid_body_info.get_world_transform body_b) *: inverted *: trans_mat in

  (* TRANSLATE: それぞれのメッシュについて、offsetを反映させた上で実行させる。 *)
  let contacts = Array.map shapes ~f:(fun shape ->
      let mesh = shape.Shape.mesh in
      Array.mapi mesh.Mesh.Mesh.facets ~f:(fun index plane ->
          if is_observe_face plane axis then
            let voronoi_region = Voronoi.voronoi_region mesh index in

            (* TRANSLATE: Body Bの各shapeにおけるそれぞれの頂点について処理をする *)
            let module RI = Realcaml_rigid_body in
            let collidable = body_b.RI.Rigid_body_info.collidable in
            let shapes = collidable.RI.Collidable.shapes in
            let points = Array.map shapes ~f:(fun shape ->
                let mesh_b = convert_vertices world_b shape.Shape.mesh in
                Array.map mesh_b ~f:(fun vert ->
                    let point = Voronoi.recent_of_region vert voronoi_region in

                    { normal = axis;
                      point_a = point;
                      point_b = vert;
                      depth = V.sub point vert |> V.norm}
                  ) |> Array.to_list
              ) |> Array.to_list in
            match List.concat points |> sort_points with
            | [] -> []
            | hd :: _  -> [hd]
          else []
        ) |> Array.to_list |> List.concat
    ) |> Array.to_list in
  match contacts with
  | [] -> []
  | _ -> List.concat contacts

(* TRANSLATE: エッジ同士における最近接点を取得する *)
let get_edge_closest_points (axis, _) body_a body_b trans_mat  =
  let module RI = Realcaml_rigid_body in
  let module RBI = RI.Rigid_body_info in
  let shapes = body_a.RBI.collidable.RI.Collidable.shapes in
  let world_b =
    let world_a = RBI.get_world_transform body_a in
    let world_b = RBI.get_world_transform body_b in
    let world_a_inv = match M.inverse world_a with
      | Some m -> m
      | None -> failwith "No have inverse matrix" in
    let open M.Open in
    world_b *: world_a_inv *: trans_mat in

  let edge_to_segment vertices edge =
    let (a, b) = edge.Mesh.Edge.vertex_ids in
    (vertices.(a), vertices.(b))
  in

  (* TRANSLATE: それぞれのメッシュについて、offsetを反映させた上で、処理の判定を行う *)
  let contacts = Array.map shapes ~f:(fun shape ->
      let mesh = shape.RI.Shape.mesh in
      let points = Array.map mesh.Mesh.Mesh.edges ~f:(fun edge_a ->
          (* TRANSLATE: Body Bの各shapeにおけるそれぞれの頂点について処理をする *)
          let points = Array.map body_b.RBI.collidable.RI.Collidable.shapes ~f:(fun shape_b ->
              let mesh_b = shape_b.RI.Shape.mesh in
              let vertices = convert_vertices world_b mesh_b in
              let points = Array.map mesh_b.Mesh.Mesh.edges ~f:(fun edge_b ->
                  let edge_a = edge_to_segment shape.RI.Shape.mesh.Mesh.Mesh.vertices edge_a
                  and edge_b = edge_to_segment vertices edge_b in
                  let module L = Typedvec.Std.Algebra.Line in
                  let e1, e2 = match L.nearest_point ~a:edge_a ~b:edge_b () with
                    | L.Collide s -> s
                    | L.Nearest s -> s
                    | L.Parallel -> failwith "edges are parallel" in
                  { normal = axis;
                    point_a = e1;
                    point_b = e2;
                    depth = V.sub e1 e2 |> V.norm}
                ) |> Array.to_list in

              match sort_points points with
              | [] -> []
              | hd :: _ -> [hd]

            ) |> Array.to_list in
          match List.concat points |> sort_points with
          | [] -> []
          | hd :: _ -> [hd]
        ) |> Array.to_list in
      match List.concat points |> sort_points with
      | [] -> []
      | hd :: _ -> [hd]
    ) |> Array.to_list in
  List.concat contacts

let get_inverse_translation axis dist =
  let module A = Typedvec.Ext.Affine in
  let axis = U.Vec.to_four axis in
  V.scalar ~scale:(dist *. 1.1) axis |> V.inverse |> A.translation_to_mat

let get_reverse_translation axis dist =
  let module A = Typedvec.Ext.Affine in
  let axis = U.Vec.to_four axis in
  V.scalar ~scale:(dist *. 1.1) axis |> A.translation_to_mat

(* TRANSLATE: 二つのbodyにおける最近接点を取得する。 *)
let get_closest_point ~axis ~dist body_a body_b =
  let offset_mat = get_inverse_translation axis dist in
  let reverse_mat = get_reverse_translation axis dist in
  let a_plane_base_closests = get_plane_closest_points (axis, dist) body_a body_b offset_mat in
  let b_plane_base_closests = get_plane_closest_points (axis, dist) body_b body_a offset_mat in
  let edge_base_closests = get_edge_closest_points (axis, dist) body_a body_b offset_mat in
  let points = List.concat [edge_base_closests;
                            a_plane_base_closests;
                            b_plane_base_closests] |> sort_points in
  match points with
  | [] -> failwith "Not found closest point"
  | hd :: _ -> 
    let open A.Open in
    let point_b = (U.Vec.to_four hd.point_b) *> reverse_mat |> U.Vec.to_three in
    {hd with point_b = point_b; depth =  dist}
