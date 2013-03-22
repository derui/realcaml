open Baselib.Std.Prelude

type t = {
  time_step:float;
  contact_bias:float;
  contact_stop:float;
  iteration:int;
  max_bodies:int;
  max_pairs:int;

  (* manage rigid body informations and collisions for them *)
  sweep_prune : SweepPrune.t;

  pair_count:int array;
  mutable pair_swap:int;
  mutable pair: Pair.t array array;
}

module RI = RigidBodyInfo
module M = Vecmath.Matrix4
module V = Vecmath.Vector

type pair_update_type = Remove | Update of (Vecmath.Vector.t * float) | New of (Vecmath.Vector.t * float)

let make ?(time_step=0.016) ?(contact_bias=0.1) ?(contact_stop=0.001)
    ?(iteration=10) ?(max_bodies=500) ?(max_pairs=5000) () =
  {time_step; contact_bias; contact_stop; iteration; max_bodies; max_pairs;
   sweep_prune = SweepPrune.make max_bodies;
   pair_swap = 0;
   pair_count = [|0; 0|];
   pair = Array.make_matrix 2 max_pairs Pair.empty;
  }

(* TRANSLATE: rigid body同士の衝突判定を行う。 *)
let intersect_bodies engine =
  let sp = engine.sweep_prune in
  let max_count = SweepPrune.get_max_count sp in
  let swap = engine.pair_swap in
  let pair_count = ref 0 in
  let pairs = engine.pair.(swap) in
  let rec intersect_loop sp pairs base_count other_count =
    if base_count >= max_count then pairs
    else if other_count >= max_count then
      intersect_loop sp pairs (succ base_count) (other_count + 2)
    else
      (* TRANSLATE: それぞれの衝突判定を行う。 *)
      match SweepPrune.intersect sp base_count other_count with
      | None -> intersect_loop sp pairs base_count (succ other_count)
      | Some (body1, body2) ->
        (* There are not enough contact point to fill contact module now,
           so contact information is not set.
        *)
        pairs.(!pair_count) <- Pair.make_by_index ~pt:Pair.New ~indexA:(Int32.of_int base_count)
          ~indexB:(Int32.of_int other_count) ();
        pair_count := succ !pair_count;
        intersect_loop sp pairs base_count (succ other_count) in
  let pairs = intersect_loop sp pairs 0 1 in
  (pairs, swap, !pair_count)

(* sort pair array by key that is swapped. *)
let sort_pair pair =
  let comp a b = compare (Pair.key a) (Pair.key b) in
  Array.stable_sort comp pair;
  pair

(* do broad phase *)
let broad_phase engine =
  let pair, swap, count = intersect_bodies engine in
  let pair = sort_pair pair in
  let old_pair = engine.pair.(engine.pair_swap) in
  let old_count = engine.pair_count.(engine.pair_swap) in

  let rec matching_loop newp oldp newc oldc =
    (* TRANSLATE: どちらかの配列を超えた場合、処理は終了とする。 *)
    if newc >= count || oldc >= old_count then newp
    else
      let new_key = Pair.key newp.(newc)
      and old_key = Pair.key oldp.(oldc) in
      let c = Int64.compare new_key old_key in
      (* TRANSLATE: 同じキーが存在している場合、前の配列のPairをKeepとして更新する *)
      if c = 0 then begin
        newp.(newc) <- Pair.change_pair oldp.(oldc) Pair.Keep;
        matching_loop newp oldp (succ newc) (succ oldc)
      end else if c < 0 then
          matching_loop newp oldp (succ newc) oldc
        else
          matching_loop newp oldp newc (succ oldc) in
  let pair = matching_loop pair old_pair 0 0 in
  engine.pair.(swap) <- pair;
  engine.pair_swap <- swap;
  engine.pair_count.(swap) <- count;
  engine

(* Get closest point from given voronoi regions with target point.  *)
let get_closest_point_with_voronoi mesh vert plane = function
  | Voronoi.Point(point) -> point
  | Voronoi.Edge(edge_point1, edge_point2) ->
    let edge = V.normalize |< V.sub edge_point2 edge_point1 in
    V.scale ~v:edge ~scale:(V.dot vert edge)
  | Voronoi.Shape ->
    let facet = Mesh.facets mesh |> flip Array.get plane in
    let normal = Mesh_facet.normal facet in
    V.sub vert (V.scale ~v:normal ~scale:(V.dot vert normal))

let is_judge_plane plane normal = V.dot (Mesh_facet.normal plane) normal >= 0.0
;;

let simple_inverse m =
  let mat3 = M.upper3x3 m |> Vecmath.Matrix3.transpose
  and vec = M.get_trans m |> V.invert in
  let trans = M.translation vec
  and rotate = M.replace_upper3x3 m mat3 in
  M.multiply trans rotate
;;

(* TRANSLATE: body_bの座標系をbody_aの座標形に変換し、衝突寸前の状態に変換する
   行列を返す。
*)
let get_coodinate_localization_matrix (axis, dist) body_a body_b =
  let world_a = RI.get_world_transform body_a
  and world_b = RI.get_world_transform body_b in
  let trans_b = M.translation (V.invert |< V.scale ~v:axis ~scale:(dist *. 1.1)) in
  let open Baselib.Std.Option.Open in
  simple_inverse world_a |> (fun mat -> M.multiply trans_b (M.multiply mat world_b))
;;

(* TODO: 最近接点を探す際、どちらか一方のどれかの形状を座標系として
   利用できるようにする。bodyが入れ替わっても、座標系は変わってはならない。
   最終的に取得した点はワールド座標系における衝突点として、それぞれの
   ローカル座標における衝突点を最終的に必要とする。ここでは、逆行列を必ず求める必要が
   あるので、簡便のために、正当ではない逆行列を返す可能性があるような関数を4x4行列に
   作っておく。
*)

let get_plane_closest_points (axis, dist) body_a body_b trans_mat =
  let shapes = Collidable.shapes |< RI.collidable body_a in
  let contacts : Vecmath.Vector.t list ref = ref [] in

  (* TRANSLATE: それぞれのメッシュについて、offsetを反映させた上で実行させる。 *)
  Array.iter (fun shape ->
    let mesh = Shape.mesh shape |> flip Mesh.transform_vertices trans_mat in
    Array.iteri (fun index plane ->
      if is_judge_plane plane axis then
        let voronoi_region = Voronoi.voronoi_region mesh index in

        (* TRANSLATE: Body Bの各shapeにおけるそれぞれの頂点について処理をする *)
        let points = Array.fold_left (fun contacts shape ->
          let mesh_b = Shape.mesh shape |> flip Mesh.transform_vertices trans_mat in
          Array.fold_left (fun contacts vert ->
            let point = get_closest_point_with_voronoi mesh vert index |<
                Voronoi.recent_of_region vert voronoi_region in
            point :: contacts
          ) [] (Mesh.vertices mesh_b)
        ) [] (Collidable.shapes |< RI.collidable body_b) in
        contacts := points @ !contacts
      else ()
    ) |< Mesh.facets mesh
  ) shapes;
  List.map (fun x -> (x, V.norm x)) !contacts
;;

(* TRANSLATE: エッジ同士における最近接点を取得する *)
let get_edge_closest_points ((axis, dist) : V.t * float)
    (body_a : RigidBodyInfo.t) (body_b : RigidBodyInfo.t) (trans_mat : M.t): (V.t * float) list =
  let shapes = Collidable.shapes |< RI.collidable body_a in
  let contacts : Vecmath.Vector.t list ref = ref [] in
  let transformed_mesh mat = flip Mesh.transform_vertices mat in
  let edge_to_segment vertices edge = let (a, b) = Mesh_edge.vertex_ids edge in
                                      Vecmath.Segment.make vertices.(a) vertices.(b) in

  (* TRANSLATE: それぞれのメッシュについて、offsetを反映させた上で、処理の果ていを行う *)
  Array.iter (fun shape ->
    let mesh = transformed_mesh trans_mat |< Shape.mesh shape in
    Array.iteri (fun index edge_a ->
        (* TRANSLATE: Body Bの各shapeにおけるそれぞれの頂点について処理をする *)
      Array.iter (fun shape_b ->
        let mesh_b = transformed_mesh trans_mat |< Shape.mesh shape_b in
        let points = Array.fold_left (fun contacts edge_b ->
          let edge_a = edge_to_segment (Shape.mesh shape |> Mesh.vertices) edge_a
          and edge_b = edge_to_segment (Shape.mesh shape_b |> Mesh.vertices) edge_b in
          let (e1, e2) = Vecmath.Segment.closest edge_a edge_b in
          e1 :: contacts
        ) [] (Mesh.edges mesh_b) in

        contacts := points @ !contacts
      ) |< (Collidable.shapes |< RI.collidable body_b)
    ) |< Mesh.edges mesh
  ) shapes;
  List.map (fun x -> (x, V.norm x)) !contacts
;;

(* TRANSLATE: 二つのbodyにおける最近接点を取得する。 *)
let get_closest_point (axis, dist) body_a body_b trans_mat =
  let plane_base_closests = get_plane_closest_points (axis, dist) body_a body_b trans_mat in
  let edge_base_closests = get_edge_closest_points (axis, dist) body_a body_b trans_mat in
  List.fold_left (fun (axis, dist) (newaxis, newdist) ->
    if dist < newdist then (axis, dist)
    else (newaxis, newdist)
  ) (List.fold_left (fun (axis, dist) (newaxis, newdist) ->
    if dist < newdist then (axis, dist)
    else (newaxis, newdist)
  ) (axis, dist) edge_base_closests) plane_base_closests
;;

let update_contact_points bodies ((axis, dist) : V.t * float) (pair : Pair.t) : Pair.t =
  (* TRANSLATE: 最近接点をpairに追加する。 *)
  let contacts = Pair.contact pair in
  let body_a = SweepPrune.get_body bodies (Int32.to_int |< Pair.indexOfA pair)
  and body_b = SweepPrune.get_body bodies (Int32.to_int |< Pair.indexOfB pair) in
  match (body_a, body_b) with
  | (None, _) | (_, None) -> failwith "body is not found..."
  | (Some(body_a), Some(body_b)) ->
    let world_a = RI.get_world_transform body_a
    and world_b = RI.get_world_transform body_b in
    let new_point = ContactPoint.make ~dist ~pointA:(M.mult_vec ~vec:axis ~mat:world_a)
      ~pointB:(M.mult_vec ~vec:axis ~mat:world_b) ~normal:axis ~constraints:[||] in
    let points_array = new_point :: (Array.to_list |< Contact.contact_points contacts) in

    if Contact.contact_num contacts < 4 then
      let friction = Contact.friction contacts in
      let module A = Baselib.Std.Array in
      Pair.update_contact pair |< Contact.make ~num:(Contact.contact_num contacts + 1)
        ~friction ~points:(Array.of_list points_array)
    else
    (* TRANSLATE:限度の個数以上になる場合、面積が最大になるような衝突点のみ選択する  *)
      let max_dist = List.fold_left (fun max_point point ->
        if ContactPoint.distance max_point >= ContactPoint.distance point then
          max_point
        else
          point
      ) new_point points_array in
      let points_without_max = Array.of_list |< List.filter (fun x ->
        ContactPoint.distance max_dist != ContactPoint.distance x) points_array in
      let combi_points = [(points_without_max.(0), points_without_max.(1),
                           points_without_max.(2));
                          (points_without_max.(0), points_without_max.(2),
                           points_without_max.(3));
                          (points_without_max.(0), points_without_max.(1),
                           points_without_max.(3));
                          (points_without_max.(1), points_without_max.(2),
                           points_without_max.(3));] in
      let calc_space a b c d =
        let a = ContactPoint.pointA a
        and b = ContactPoint.pointA b
        and c = ContactPoint.pointA c
        and d = ContactPoint.pointA d in
        List.sort compare [V.cross (V.sub a c) (V.sub b d) |> V.norm |> abs_float;
                           V.cross (V.sub a b) (V.sub d c) |> V.norm |> abs_float;
                           V.cross (V.sub a d) (V.sub b c) |> V.norm |> abs_float;] |> List.hd in
    (* TRANSLATE: 最大のdistを中心として、残りを組み合わせてチェックする *)
      let ((a, b, c, d), _) =
        (List.map (fun (b, c, d) -> ((max_dist,b,c,d), calc_space max_dist b c d))
          combi_points) |>
              List.sort (fun (_, square1) (_, square2) -> compare square1 square2) |> List.hd in
      let friction = Contact.friction contacts in
      let module A = Baselib.Std.Array in
      Pair.update_contact pair |< Contact.make ~num:(Contact.contact_num contacts)
        ~friction ~points:[|a;b;c;d|]
;;

(* do narrow phase *)
let narrow_phase engine =
  (* TRANSLATE: 各ペア同士について、衝突点を計算する *)
  let current_pair = engine.pair.(engine.pair_swap)
  and bodies = engine.sweep_prune in
  let is_separate body_a body_b =
    SeparatingAxis.judge_intersect ~body_a ~body_b in

  (* TRANSLATE: あるペアにおける衝突点を計算する *)
  let solve_contact_point index pair =
    let body_a = SweepPrune.get_body bodies (Int32.to_int |< Pair.indexOfA pair)
    and body_b = SweepPrune.get_body bodies (Int32.to_int |< Pair.indexOfB pair) in
    let open SeparatingAxis in
    match (body_a, body_b) with
    | (None, _) | (_, None) -> failwith "not found one or two of pair"
    | (Some(body_a), Some(body_b)) ->
      match is_separate body_a body_b  with
    (* TRANSLATE: APlaneの場合、body Aを基準として判定する。  *)
      | Some (APlane, axis, dist) ->
        let a_to_b = get_coodinate_localization_matrix (axis, dist) body_a body_b in
        Some(get_closest_point (axis, dist) body_a body_b a_to_b)
    (* TRANSLATE: BPlaneの場合、body Bを基準として判定する。  *)
      | Some (BPlane, axis, dist) ->
        let a_to_b = get_coodinate_localization_matrix (axis, dist) body_a body_b in
        Some(get_closest_point (axis, dist) body_b body_a a_to_b)
    (* TRANSLATE: Edgeの場合、body Aを基準として判定する。  *)
      | Some (Edge, axis, dist) ->
        let a_to_b = get_coodinate_localization_matrix (axis, dist) body_a body_b in
        Some(get_closest_point (axis, dist) body_a body_b a_to_b)
    (* TRANSLATE: 分離平面が存在する場合には、このペアに対して何も行わない *)
      | None -> None in

  let updated_pair = Array.mapi (fun index pair ->
    match solve_contact_point index pair with
    | None -> pair
    | Some((axis, dist)) ->
    (* TRANSLATE: ここで取得した衝突点は、すべて剛体Aを基準にとったものとなるため、
       一度ワールド座標系に衝突点を変換する。
    *)
    match SweepPrune.get_body bodies (Int32.to_int |< Pair.indexOfA pair) with
    | None -> failwith "body A not found"
    | Some(body_a) ->
      let to_world = RI.get_world_transform body_a |> simple_inverse in
      let axis = M.mult_vec ~mat:to_world ~vec:axis in

      update_contact_points bodies (axis, dist) pair
  ) current_pair in
  engine.pair.(engine.pair_swap) <- updated_pair;
  engine

(* do solve constarints *)
let solve_constraints engine = ()

let execute_pipeline engine = ()
