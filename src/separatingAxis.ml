open Sugarpot.Std.Prelude

open Candyvec.Std

module R = RigidBodyInfo
module V = Vector

type depth = float
type separating_type = Edge             (* Edge to edge *)
                       | APlane         (* A plane of the body A *)
                       | BPlane         (* A plane of the body B *)
type separating_axis = separating_type * V.t * depth

type mesh_info = Mesh.t * Candyvec.Std.Matrix4.t

module Base = struct
  (* TRANSLATE: ある軸上にメッシュを投影した場合の最大値と最小値を取得する *)
  let get_maximum_range axis vertices =
    Array.fold_left (fun (pmax, pmin) elem ->
      let projected = V.dot elem axis in
      (max pmax projected, min pmin projected)
    ) (min_float, max_float) vertices

  let detect_separation sep_axis (amax, amin) (bmax, bmin) =
    let d1 = amin -. bmax
    and d2 = bmin -. amax in

    if d1 >= 0.0 || d2 >= 0.0 then None
    else if d1 < d2 then Some (Vector.invert sep_axis, d2)
    else Some (sep_axis, d1)
end

(* TRANSLATE: Noneを返した時点で終了するArray.fold_left *)
let breakable_fold ary f init =
  let length = Array.length ary in
  let rec loop index f prev =
    let open Sugarpot.Std.Option.Open in
    if index >= length then Some prev
    else
      f prev ary.(index) >>= loop (succ index) f
  in
  loop 0 f init

(* TRANSLATE: AとBの間に、sep_axisを分離軸として分離平面が存在するかどうかを調べる *)
let is_separate_axis ~info_a:(mesh_a, world_a) ~info_b:(mesh_b, world_b) ~sep_axis =
  let open Sugarpot.Std.Option.Open in
  Matrix4.inverse world_b >>= (fun inversed -> 
    let open Candyvec.Std.Matrix4.Open in
    (* TRANSLATE: Aのローカル座標系からBのローカル座標形への変換行列 *)
    let trans_a2b = world_a *|> inversed in

    let sep_axisb = sep_axis *||> trans_a2b |> V.normalize in
    let offset_vec = V.sub sep_axis sep_axisb in
    (* TRANSLATE: shape_bをAのローカル座標系に変換すると、演算負荷が高いため、
       分離軸をBのローカル座標として扱い、取得した値をAのローカル座標系に
       換算することで、演算負荷を抑えることができる。
    *)
    let offset = Vector.dot offset_vec sep_axis in
    let (amax, amin) = Base.get_maximum_range sep_axis mesh_a.Mesh.vertices
    and (bmax, bmin) = Base.get_maximum_range sep_axisb mesh_b.Mesh.vertices in
    let (bmax, bmin) = (bmax +. offset, bmin +. offset) in
    Base.detect_separation sep_axis (amax, amin) (bmax, bmin)
  )

(* TRANSLATE: 各面法線を分離軸として判定する *)
let face_intersect (shape_a, world_a) (shape_b, world_b) (styp, axis, dist) septype =
  let faces = match septype with
    | APlane -> shape_a.Shape.mesh.Mesh.facets
    | BPlane -> shape_b.Shape.mesh.Mesh.facets
    | _ -> failwith "face_intersect allowed only APlane or BPlane separation type"
  in
  let mesh_a = shape_a.Shape.mesh 
  and mesh_b = shape_b.Shape.mesh in
  let per_face (styp, axis, dist) face =
    let sep_axis = face.Mesh.Facet.normal in
    match is_separate_axis ~info_a:(mesh_a, world_a) ~info_b:(mesh_b, world_b) ~sep_axis with
    | None -> None
    | Some (newaxis, newdist) ->
      if newdist < dist then Some (styp, axis, dist) else Some (septype, newaxis, newdist) in
  breakable_fold faces per_face (septype, axis, dist)

(* TRANSLATE: 各エッジ同士の外積を分離軸として判定する。 *)
let edge_intersect (shape_a, world_a) (shape_b, world_b) (styp, axis, dist) =
  let mesh_a = shape_a.Shape.mesh 
  and mesh_b = shape_b.Shape.mesh in
  let vertices_a = mesh_a.Mesh.vertices
  and vertices_b = mesh_b.Mesh.vertices 
  and edges_a = mesh_a.Mesh.edges
  and edges_b = mesh_b.Mesh.edges in
  let edge_to_vec edge vertices =
    let first, second = edge.Mesh.Edge.vertex_ids in
    Vector.sub vertices.(second) vertices.(first) in

  let module V = Candyvec.Std.Vector in
  let open Candyvec.Std.Matrix4.Open in
  let open Sugarpot.Std.Option.Open in
  breakable_fold edges_a (fun (styp, axis, dist) edge ->
    let edge_a = edge_to_vec edge vertices_a in
    breakable_fold edges_b (fun (styp, axis, dist) edge ->
      let edge_b = edge_to_vec edge vertices_b in
      let separation = Matrix4.inverse world_a >>= (fun inverse ->
        let edge_b = edge_b *||> world_b *||> inverse in
        let sep_axis = Vector.cross edge_a edge_b |> Vector.normalize in
        is_separate_axis ~info_a:(mesh_a, world_a) ~info_b:(mesh_b, world_b) ~sep_axis
      ) in
      match separation with
      | None -> None
      | Some (newaxis, newdist) ->
        (* TRANSLATE: 貫通深度が最も浅い部分を取得する *)
        if newdist < dist then Some (styp, axis, dist) else Some (Edge, newaxis, newdist)
    ) (styp, axis, dist)
  ) (styp, axis, dist)

let to_shape_info body =
  let state = body.R.state in
  let world = State.to_world_transform state in
  (body.R.collidable.Collidable.shapes, world)

let judge_intersect ~body_a ~body_b =
  let open Candyvec in

  let (shapes_a, world_a) = to_shape_info body_a
  and (shapes_b, world_b) = to_shape_info body_b in
  let length_a = Array.length shapes_a
  and length_b = Array.length shapes_b in
  let sep_init = (APlane, Vector.zero, -. max_float) in
  let open Sugarpot.Std.Option.Open in
  let rec intersect_loop ind_a ind_b =
    if ind_a >= length_a then None
    else if ind_b >= length_b then intersect_loop (succ ind_a) 0
    else
      let shape_a = (shapes_a.(ind_a), world_a)
      and shape_b = (shapes_b.(ind_b), world_b) in

      (* TRANSLATE: Aの各面法線ベクトル、Bの各面法線ベクトル、各Edgeの外積を
         分離軸として判定する。どこかで分離平面が見付かれば、その時点で判定は終了する。
      *)
      (face_intersect shape_a shape_b sep_init APlane) >>=
        (fun sep_axis ->
          face_intersect shape_a shape_b sep_axis BPlane) >>=
        (fun sep_axis -> edge_intersect shape_a shape_b sep_axis) in
  intersect_loop 0 0
