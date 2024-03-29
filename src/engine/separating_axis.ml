open Core
module A = Typedvec.Algebra
module S = Typedvec.Size
module R = Realcaml_rigid_body.Rigid_body_info
module V = A.Vec
module M = A.Mat
module U = Realcaml_util

type depth = float

type separating_type =
  | Edge (* Edge to edge *)
  | APlane (* A plane of the body A *)
  | BPlane
(* A plane of the body B *)

type separating_axis = separating_type * U.Types.vec * depth

(* TRANSLATE: ある軸上にメッシュを投影した場合の最大値と最小値を取得する *)
let get_maximum_range axis vertices =
  Array.fold vertices
    ~f:(fun (pmax, pmin) elem ->
      let projected = V.dot elem axis in
      (Float.max pmax projected, Float.min pmin projected))
    ~init:(Float.min_value, Float.max_value)

let detect_separation sep_axis (amax, amin) (bmax, bmin) =
  let open Float in
  let d1 = amin - bmax and d2 = bmin - amax in

  if d1 >= 0.0 || d2 >= 0.0 then None else if d1 < d2 then Some (V.inverse sep_axis, d2) else Some (sep_axis, d1)

(* TRANSLATE: AとBの間に、sep_axisを分離軸として分離平面が存在するかどうかを調べる *)
let is_separate_axis ~info_a:(mesh_a, state_a) ~info_b:(mesh_b, state_b) ~sep_axis () =
  let open Option in
  let module State = Realcaml_rigid_body.State in
  let world_a = State.to_world_transform state_a in
  State.to_world_transform state_b |> M.inverse >>= fun inversed ->
  let open M.Open in
  let open A.Open in
  (* TRANSLATE: Aのローカル座標系からBのローカル座標形への変換行列 *)
  let trans_a2b = world_a *: inversed in
  let sep_axisb = U.Vec.to_four sep_axis *> trans_a2b |> U.Vec.to_three |> V.normalize in
  M.inverse world_a >>= fun inversed ->
  let module AF = Typedvec.Ext.Affine in
  let offset_on_a = U.Vec.with_four ~f:(fun p -> p *> inversed) state_b.State.pos in

  (* TRANSLATE: shape_bをAのローカル座標系に変換すると、演算負荷が高いため、 分離軸をBのローカル座標として扱い、取得した値をAのローカル座標系に 換算することで、演算負荷を抑えることができる。 *)
  let offset = V.dot offset_on_a sep_axis in
  let module Mesh = Realcaml_mesh.Mesh in
  let amax, amin = get_maximum_range sep_axis mesh_a.Mesh.vertices
  and bmax, bmin = get_maximum_range sep_axisb mesh_b.Mesh.vertices in
  let bmax, bmin = (bmax +. offset, bmin +. offset) in
  detect_separation sep_axis (amax, amin) (bmax, bmin)

(* TRANSLATE: 各面法線を分離軸として判定する *)
let face_intersect (shape_a, state_a) (shape_b, state_b) sep septype =
  let module State = Realcaml_rigid_body.State in
  let module Shape = Realcaml_rigid_body.Shape in
  let module Mesh = Realcaml_mesh.Mesh in
  let module Facet = Realcaml_mesh.Facet in
  let faces =
    match septype with
    | APlane -> shape_a.Shape.mesh.Mesh.facets
    | BPlane -> shape_b.Shape.mesh.Mesh.facets
    | _      -> failwith "face_intersect allowed only APlane or BPlane separation type"
  in
  let mesh_a = shape_a.Shape.mesh and mesh_b = shape_b.Shape.mesh in
  let open Option in
  let per_face sep face =
    sep >>= fun (styp, axis, dist) ->
    let sep_axis = face.Facet.normal in
    let open Float in
    match is_separate_axis ~info_a:(mesh_a, state_a) ~info_b:(mesh_b, state_b) ~sep_axis () with
    | None                    -> None
    | Some (newaxis, newdist) -> if newdist < dist then Some (styp, axis, dist) else Some (septype, newaxis, newdist)
  in
  Array.fold faces ~f:per_face ~init:(Some sep)

(* TRANSLATE: 各エッジ同士の外積を分離軸として判定する。 *)
let edge_intersect (shape_a, state_a) (shape_b, state_b) (styp, axis, dist) =
  let module Shape = Realcaml_rigid_body.Shape in
  let module Mesh = Realcaml_mesh.Mesh in
  let module Facet = Realcaml_mesh.Facet in
  let module Edge = Realcaml_mesh.Edge in
  let mesh_a = shape_a.Shape.mesh and mesh_b = shape_b.Shape.mesh in
  let vertices_a = mesh_a.Mesh.vertices
  and vertices_b = mesh_b.Mesh.vertices
  and edges_a = mesh_a.Mesh.edges
  and edges_b = mesh_b.Mesh.edges in
  let edge_to_vec edge vertices =
    let open V.Open in
    let first, second = edge.Edge.vertex_ids in
    vertices.(second) -: vertices.(first)
  in

  let open Option in
  let open A.Open in
  let module State = Realcaml_rigid_body.State in
  let world_a = State.to_world_transform state_a in
  let world_b = State.to_world_transform state_b in

  Array.fold edges_a
    ~init:(Some (styp, axis, dist))
    ~f:(fun sep edge ->
      sep >>= fun (styp, axis, dist) ->
      let edge_a = edge_to_vec edge vertices_a in
      Array.fold edges_b
        ~init:(Some (styp, axis, dist))
        ~f:(fun sep edge ->
          sep >>= fun (styp, axis, dist) ->
          let edge_b = edge_to_vec edge vertices_b in
          ( M.inverse world_a >>= fun inverse ->
            let open M.Open in
            let edge_b = U.Vec.to_four edge_b *> (world_b *: inverse) |> U.Vec.to_three in
            let sep_axis = V.cross ~left:edge_b ~right:edge_a |> V.normalize in
            is_separate_axis ~info_a:(mesh_a, state_a) ~info_b:(mesh_b, state_b) ~sep_axis () )
          >>= fun (newaxis, newdist) ->
          (* TRANSLATE: 貫通深度が最も浅い部分を取得する *)
          let open Float in
          if newdist < dist then Some (styp, axis, dist) else Some (Edge, newaxis, newdist)))

(* あるbodyを、ワールド変換行列とBodyを構成するShapesに変換する *)
let to_shape_info body =
  let module S = Realcaml_rigid_body.State in
  let module C = Realcaml_rigid_body.Collidable in
  let state = body.R.state in
  (body.R.collidable.C.shapes, state)

let judge_intersect ~body_a ~body_b =
  let shapes_a, world_a = to_shape_info body_a and shapes_b, world_b = to_shape_info body_b in
  let length_a = Array.length shapes_a and length_b = Array.length shapes_b in
  let sep_init = (APlane, U.Vec.empty (), -.Float.max_value) in
  let rec intersect_loop ind_a ind_b =
    if ind_a >= length_a then None
    else if ind_b >= length_b then intersect_loop (succ ind_a) 0
    else
      let open Option in
      let shape_a = (shapes_a.(ind_a), world_a) and shape_b = (shapes_b.(ind_b), world_b) in

      (* TRANSLATE: Aの各面法線ベクトル、Bの各面法線ベクトル、各Edgeの外積を 分離軸として判定する。どこかで分離平面が見付かれば、その時点で判定は終了する。 *)
      (face_intersect shape_a shape_b sep_init APlane >>= fun sep_axis -> face_intersect shape_b shape_a sep_axis BPlane)
      >>= fun sep_axis -> edge_intersect shape_a shape_b sep_axis
  in
  intersect_loop 0 0
