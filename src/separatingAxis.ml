open Baselib.Std.Prelude

open Vecmath

module R = RigidBodyInfo

type depth = float
type separating_axis = Vecmath.Vector.t * depth

(* TRANSLATE: ある軸上にメッシュを投影した場合の最大値と最小値を取得する *)
let get_projection axis mesh =
  let vertices = Mesh.vertices mesh in

  Array.fold_left (fun (pmax, pmin) elem ->
    let projected = Vecmath.Vector.dot elem axis in
    (max pmax projected, min pmin projected)
  ) (min_float, max_float) vertices

(* TRANSLATE: 姿勢と位置から、ワールド変換行列を作成する *)
let world_transform orient vec =
  let open Vecmath in
  let orient = Quaternion.to_matrix orient
  and trans = Matrix4.translation vec in
  Matrix4.multiply trans orient

(* TRANSLATE: Noneを返した時点で終了するArray.fold_left *)
let breakable_fold ary f init =
  let length = Array.length ary in
  let rec loop index f prev =
    if index >= length then None
    else
      match f prev ary.(index) with
      | None -> None
      | Some x -> loop (succ index) f x in
  loop 0 f init

(* TRANSLATE: AとBの間に、sep_axisを分離軸として分離平面が存在するかどうかを調べる *)
let is_separate_axis ~info_a:(mesh_a, world_a) ~info_b:(mesh_b, world_b) ~sep_axis =
  match Matrix4.inverse world_b with
  | None -> None
  | Some inversed ->
    (* TRANSLATE: Bのローカル座標系からAのローカル座標形への変換行列 *)
    let trans_a2b = Matrix4.multiply inversed world_a in

    let sep_axisb = Matrix4.mult_vec trans_a2b sep_axis in
    let offset_vec = Vector.sub (Matrix4.mult_vec world_a sep_axis)
      (Matrix4.mult_vec world_b sep_axisb) in
    (* TRANSLATE: shape_bをAのローカル座標系に変換すると、演算負荷が高いため、
       分離軸をBのローカル座標として扱い、取得した値をAのローカル座標系に
       換算することで、演算負荷を抑えることができる。
    *)
    let offset = Vector.dot offset_vec sep_axis in
    let (amax, amin) = get_projection sep_axis mesh_a
    and (bmax, bmin) = get_projection sep_axisb mesh_b in
    let (bmax, bmin) = (bmax +. offset, bmin +. offset) in
    let d1 = amin -. bmax
    and d2 = bmin -. amax in

    if d1 >= 0.0 || d2 >= 0.0 then None
    else if d1 < d2 then Some (Vector.invert sep_axis, d2)
    else Some (sep_axis, d1)

let judge_intersect ~body_a ~body_b =
  let open Vecmath in
  let state_a = R.state body_a
  and state_b = R.state body_b in
  let world_a = world_transform (State.orientation state_a) (State.pos state_a)
  and world_b = world_transform (State.orientation state_b) (State.pos state_b) in

  let shapes_a = R.collidable body_a |> Collidable.shapes
  and shapes_b = R.collidable body_b |> Collidable.shapes in
  let length_a = Array.length shapes_a
  and length_b = Array.length shapes_b in

  (* TRANSLATE: 各面法線を分離軸として判定する *)
  let face_intersect faces shape_a shape_b =
    let mesh_a = Shape.mesh shape_a
    and mesh_b = Shape.mesh shape_b in
    let per_face (axis, dist) face =
      let sep_axis = Mesh_facet.normal face in
      match is_separate_axis ~info_a:(mesh_a, world_a) ~info_b:(mesh_b, world_b) ~sep_axis with
      | None -> None
      | Some (newaxis, newdist) ->
        if newdist < dist then Some (axis, dist) else Some (newaxis, newdist) in
    breakable_fold faces per_face (Vector.zero, min_float) in

  (* TRANSLATE: 各エッジ同士の外積を分離軸として判定する。 *)
  let edge_intersect shape_a shape_b =
    let mesh_a = Shape.mesh shape_a
    and mesh_b = Shape.mesh shape_b in
    let vertices_a = Mesh.vertices mesh_a
    and vertices_b = Mesh.vertices mesh_b
    and edges_a = Mesh.edges mesh_a
    and edges_b = Mesh.edges mesh_b in
    let edge_to_vec edge vertices =
      let first, second = Mesh_edge.vertex_ids edge in
      Vector.sub vertices.(second) vertices.(first) in

    breakable_fold edges_a (fun (axis, dist) edge ->
      let edge_a = edge_to_vec edge vertices_a in
      breakable_fold edges_b (fun (axis, dist) edge ->
        let edge_b = edge_to_vec edge vertices_b in
        let sep_axis = Vector.cross edge_a edge_b |> Vector.normalize in
        match is_separate_axis ~info_a:(mesh_a, world_a) ~info_b:(mesh_b, world_b) ~sep_axis with
        | None -> None
        | Some (newaxis, newdist) ->
          (* TRANSLATE: 貫通深度が最も浅い部分を取得する *)
          if newdist < dist then Some (axis, dist) else Some (newaxis, newdist)
      ) (axis, dist)
    ) (Vector.zero, min_float) in

  let open Baselib.Std.Option.Open in
  let rec intersect_loop ind_a ind_b =
    if ind_a >= length_a then None
    else if ind_b >= length_b then intersect_loop (succ ind_a) 0
    else
      let shape_a = shapes_a.(ind_a)
      and shape_b = shapes_b.(ind_b) in

      (* TRANSLATE: Aの各面法線ベクトル、Bの各面法線ベクトル、各Edgeの外積を
         分離軸として判定する。どこかで分離平面が見付かれば、その時点で判定は終了する。
      *)
      face_intersect (Shape.mesh shape_a |> Mesh.facets) shape_a shape_b >>=
        (fun _ -> face_intersect (Shape.mesh shape_b |> Mesh.facets) shape_a shape_b) >>=
        (fun _ -> edge_intersect shape_a shape_b) in
  intersect_loop 0 0
