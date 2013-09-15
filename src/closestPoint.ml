module V = Candyvec.Std.Vector
open Sugarpot.Std.Prelude
module RI = RigidBodyInfo
module M = Candyvec.Matrix4
module Q = Candyvec.Quaternion

module Base = struct
  let is_observe_face plane normal = V.dot plane.Mesh.Facet.normal normal >= 0.0
(* Check the plane to be equal direction for normal. *)

(* TODO: 最近接点を探す際、どちらか一方のどれかの形状を座標系として
   利用できるようにする。bodyが入れ替わっても、座標系は変わってはならない。
   最終的に取得した点はワールド座標系における衝突点として、それぞれの
   ローカル座標における衝突点を最終的に必要とする。ここでは、逆行列を必ず求める必要が
   あるので、簡便のために、正当ではない逆行列を返す可能性があるような関数を4x4行列に
   作っておく。
*)
  let get_plane_closest_points (axis, dist) body_a body_b trans_mat =
    let shapes = body_a.RI.collidable.Collidable.shapes in
    let contacts : Candyvec.Vector.t list ref = ref [] in

  (* TRANSLATE: それぞれのメッシュについて、offsetを反映させた上で実行させる。 *)
    Array.iter (fun shape ->
      let mesh = shape.Shape.mesh |> flip Mesh.transform_vertices trans_mat in
      Array.iteri (fun index plane ->
        if is_observe_face plane axis then
          let voronoi_region = Voronoi.voronoi_region mesh index in

        (* TRANSLATE: Body Bの各shapeにおけるそれぞれの頂点について処理をする *)
          let points = Array.fold_left (fun contacts shape ->
            let mesh_b = shape.Shape.mesh |> flip Mesh.transform_vertices trans_mat in
            Array.fold_left (fun contacts vert ->
              let point = Voronoi.expand_recent_point |<
                  Voronoi.recent_of_region vert voronoi_region in

              point :: contacts
            ) [] mesh_b.Mesh.vertices
          ) [] body_b.RI.collidable.Collidable.shapes in
          contacts := points @ !contacts
        else ()
      ) mesh.Mesh.facets
    ) shapes;
    List.map (fun x -> (V.normalize x, V.norm x)) !contacts


(* TRANSLATE: エッジ同士における最近接点を取得する *)
  let get_edge_closest_points ((axis, dist) : V.t * float)
      (body_a : RigidBodyInfo.t) (body_b : RigidBodyInfo.t) (trans_mat : M.t): (V.t * float) list =
    let shapes = body_a.RI.collidable.Collidable.shapes in
    let contacts : Candyvec.Vector.t list ref = ref [] in
    let transform_mesh mat = flip Mesh.transform_vertices mat in
    let edge_to_segment vertices edge =
      let (a, b) = edge.Mesh.Edge.vertex_ids in
      Candyvec.Segment.make vertices.(a) vertices.(b)
    in

  (* TRANSLATE: それぞれのメッシュについて、offsetを反映させた上で、処理の果ていを行う *)
    Array.iter (fun shape ->
      let mesh = transform_mesh trans_mat shape.Shape.mesh in
      Array.iteri (fun index edge_a ->
      (* TRANSLATE: Body Bの各shapeにおけるそれぞれの頂点について処理をする *)
        Array.iter (fun shape_b ->
          let mesh_b = transform_mesh trans_mat shape_b.Shape.mesh in
          let points = Array.fold_left (fun contacts edge_b ->
            let edge_a = edge_to_segment shape.Shape.mesh.Mesh.vertices edge_a
            and edge_b = edge_to_segment shape_b.Shape.mesh.Mesh.vertices edge_b in
            let (e1, e2) = Candyvec.Segment.closest edge_a edge_b in
            e1 :: contacts
          ) [] mesh_b.Mesh.edges in

          contacts := points @ !contacts
        ) |< body_b.RI.collidable.Collidable.shapes
      ) mesh.Mesh.edges
    ) shapes;
    List.map (fun x -> (V.normalize x, V.norm x)) !contacts

end

let get_coodinate_localization_matrix (axis, dist) body_a body_b =
  let world_a = RI.get_world_transform body_a
  and world_b = RI.get_world_transform body_b in
  let trans_b = M.translation (V.invert |< V.scale ~v:axis ~scale:(dist *. 1.1)) in
  let open Sugarpot.Std.Option.Open in
  M.force_inverse world_a |> (fun mat -> M.multiply trans_b (M.multiply mat world_b))

(* TRANSLATE: 二つのbodyにおける最近接点を取得する。 *)
let get_closest_point (axis, dist) body_a body_b =
  let trans_mat = get_coodinate_localization_matrix (axis, dist) body_a body_b in
  let plane_base_closests = Base.get_plane_closest_points (axis, dist) body_a body_b trans_mat in
  let edge_base_closests = Base.get_edge_closest_points (axis, dist) body_a body_b trans_mat in
  List.fold_left (fun (axis, dist) (newaxis, newdist) ->
    if dist < newdist then (axis, dist)
    else (newaxis, newdist)
  ) (List.fold_left (fun (axis, dist) (newaxis, newdist) ->
    if dist < newdist then (axis, dist)
    else (newaxis, newdist)
  ) (axis, dist) edge_base_closests) plane_base_closests

