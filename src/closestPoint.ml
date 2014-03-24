open Sugarpot.Std.Prelude
module RI = RigidBodyInfo
module M = Candyvec.Std.Matrix
module MU = Candyvec.Std.Matrix_util
module V = Candyvec.Std.Vector
module Q = Candyvec.Std.Quaternion

module Base = struct

  (* Result of functions in this module. That types are each positions to first and second mesh,
     and last float is distination between them.
  *)
  type t = {
    normal : V.t;
    point_a : V.t;
    point_b : V.t;
    depth : float
  }

  let sort_points list = List.sort (fun {depth = d1;_} {depth = d2;_} -> compare d1 d2) list

  let is_observe_face plane normal = V.dot plane.Mesh.Facet.normal normal >= 0.0
  (* Check the plane to be equal direction for normal. *)

  (* TODO: 最近接点を探す際、どちらか一方のどれかの形状を座標系として
     利用できるようにする。bodyが入れ替わっても、座標系は変わってはならない。
     最終的に取得した点はワールド座標系における衝突点として、それぞれの
     ローカル座標における衝突点を最終的に必要とする。ここでは、逆行列を必ず求める必要が
     あるので、簡便のために、正当ではない逆行列を返す可能性があるような関数を4x4行列に
     作っておく。
  *)
  let get_plane_closest_points (axis, _) body_a body_b trans_mat =
    let shapes = body_a.RI.collidable.Collidable.shapes in
    let open Candyvec.Std.Matrix.Open in
    let world_b =
      let world_a = RI.get_world_transform body_a in
      RI.get_world_transform body_b *|> MU.force_inverse world_a *|> trans_mat in

    (* TRANSLATE: それぞれのメッシュについて、offsetを反映させた上で実行させる。 *)
    let contacts = Array.to_list |< Array.map (fun shape ->
      let mesh = shape.Shape.mesh in
      Array.to_list |< Array.mapi (fun index plane ->
        if is_observe_face plane axis then
          let voronoi_region = Voronoi.voronoi_region mesh index in

          (* TRANSLATE: Body Bの各shapeにおけるそれぞれの頂点について処理をする *)
          let points = Array.to_list |< Array.map (fun shape ->
            let mesh_b = shape.Shape.mesh |> flip Mesh.transform_vertices world_b in
            Array.to_list |< Array.map (fun vert ->
              let point = Voronoi.recent_of_region vert voronoi_region |>
                  Voronoi.expand_recent_point in

              { normal = axis;
                point_a = point;
                point_b = vert;
                depth = V.sub point vert |> V.norm}
            ) mesh_b.Mesh.vertices
          ) body_b.RI.collidable.Collidable.shapes in
          match points with
          | [] -> None
          | _ -> Some (List.concat points |> sort_points |> List.hd)
        else None
      ) mesh.Mesh.facets
    ) shapes in
    let module O = Sugarpot.Std.Option in
    match contacts with
    | [] -> []
    | _ -> List.concat contacts |> O.option_map id

  (* TRANSLATE: エッジ同士における最近接点を取得する *)
  let get_edge_closest_points (axis, _)
      (body_a : RigidBodyInfo.t) (body_b : RigidBodyInfo.t) (trans_mat : M.t): t list =
    let shapes = body_a.RI.collidable.Collidable.shapes in
    let open Candyvec.Std.Matrix.Open in
    let world_b =
      let world_a = RI.get_world_transform body_a in
      RI.get_world_transform body_b *|> MU.force_inverse world_a *|> trans_mat in

    let transform_mesh mat = flip Mesh.transform_vertices mat in
    let edge_to_segment vertices edge =
      let (a, b) = edge.Mesh.Edge.vertex_ids in
      Candyvec.Segment.make vertices.(a) vertices.(b)
    in

    let module O = Sugarpot.Std.Option in
    (* TRANSLATE: それぞれのメッシュについて、offsetを反映させた上で、処理の果ていを行う *)
    let contacts = Array.to_list |< Array.map (fun shape ->
      let mesh = shape.Shape.mesh in
      let points = Array.to_list |< Array.map (fun edge_a ->
        (* TRANSLATE: Body Bの各shapeにおけるそれぞれの頂点について処理をする *)
        let points = Array.to_list |< Array.map (fun shape_b ->
          let mesh_b = transform_mesh world_b shape_b.Shape.mesh in
          let points = Array.to_list |< Array.map (fun edge_b ->
            let edge_a = edge_to_segment shape.Shape.mesh.Mesh.vertices edge_a
            and edge_b = edge_to_segment mesh_b.Mesh.vertices edge_b in
            let e1, e2 = Candyvec.Segment.closest edge_a edge_b in
            Printf.printf "closests : %s %s %s\n" (V.to_string axis) (V.to_string e1) (V.to_string e2);
            { normal = axis;
              point_a = e1;
              point_b = e2;
              depth = V.sub e1 e2 |> V.norm}
          ) mesh_b.Mesh.edges in

          match points with
          | [] -> None
          | _ -> Some (sort_points points |> List.hd)

        ) body_b.RI.collidable.Collidable.shapes in
        match O.option_map id points with
        | [] -> None
        | points -> Some (sort_points points |> List.hd)
      ) mesh.Mesh.edges in
      match O.option_map id points with
      | [] -> None
      | points -> Some (sort_points points |> List.hd)
    ) shapes in
    O.option_map id contacts
end

let get_inverse_translation axis dist =
  M.translation (V.invert |< V.scale ~v:axis ~scale:(dist *. 1.1))

let get_reverse_translation axis dist =
  M.translation (V.scale ~v:axis ~scale:(dist *. 1.1))

(* TRANSLATE: 二つのbodyにおける最近接点を取得する。 *)
let get_closest_point (axis, dist) body_a body_b =
  let offset_mat = get_inverse_translation axis dist in
  let reverse_mat = get_reverse_translation axis dist in
  let a_plane_base_closests = Base.get_plane_closest_points (axis, dist) body_a body_b offset_mat in
  let b_plane_base_closests = Base.get_plane_closest_points (axis, dist) body_b body_a offset_mat in
  let edge_base_closests = Base.get_edge_closest_points (axis, dist) body_a body_b offset_mat in
  let point = List.concat [edge_base_closests;
                           a_plane_base_closests;
                           b_plane_base_closests] |> Base.sort_points |> List.hd in
  let open Candyvec.Std.Matrix.Open in
  let point_b =point.Base.point_b *||> reverse_mat in
  {point with Base.point_b = point_b; depth =  dist}

include Base
