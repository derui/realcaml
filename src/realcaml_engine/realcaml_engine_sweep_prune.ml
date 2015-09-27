open Core.Std
module Rigid_body = Realcaml_rigid_body
module A = Typedvec.Std.Algebra
module RBI = Rigid_body.Rigid_body_info
module M = A.Mat
module Q = Typedvec.Ext.Qua
module U = Realcaml_util

type t = {
  bodies:RBI.t option array;
  current_count: int;
}

type intersect = RBI.t * RBI.t

let make count =
  {current_count = 0; bodies = Array.create count None;}

let add prune info =
  let current = prune.current_count in
  prune.bodies.(current) <- Some info;
  {prune with current_count = succ current}

let vec_to_translated v world =
  let open A.Open in
  let open M.Open in
  let module AF = Typedvec.Ext.Affine in
  let module V = A.Vec in
  let trans = (AF.translation (U.Vec.to_four v)) *: world in
  U.Vec.four_empty () *> trans |> U.Vec.to_three

let intersect info ind_a ind_b =
  let module C = Rigid_body.Collidable in
  let module AABB = Realcaml_engine_AABB in
  let in_range i range = i >= 0 && i < range in
  if not (in_range ind_a info.current_count) ||
     not (in_range ind_b info.current_count) then None
  else
    match (info.bodies.(ind_a), info.bodies.(ind_b)) with
    | (None, _) | (_, None) -> None
    | (Some a_inf, Some b_inf) ->
      let a_trans = RBI.get_world_transform a_inf
      and b_trans = RBI.get_world_transform b_inf in

      let a_pos = vec_to_translated a_inf.RBI.collidable.C.center a_trans
      and a_size = a_inf.RBI.collidable.C.half_size
      and b_pos = vec_to_translated b_inf.RBI.collidable.C.center b_trans
      and b_size = b_inf.RBI.collidable.C.half_size in

      let ax, ay, az = U.Vec.extract_three a_pos
      and asx, asy, asz = U.Vec.extract_three a_size
      and bx, by, bz = U.Vec.extract_three b_pos
      and bsx, bsy, bsz = U.Vec.extract_three b_size in
      (* TRANSLATE: X軸から衝突をチェックしてみる *)
      if AABB.intersect_one_axis ~pos_a:ax ~len_a:asx ~pos_b:bx ~len_b:bsx () then
        (* TRANSLATE: X軸で交差していた場合、残り二軸についても衝突判定する。 *)
        let iy = AABB.intersect_one_axis ~pos_a:ay ~len_a:asy ~pos_b:by ~len_b:bsy ()
        and iz = AABB.intersect_one_axis ~pos_a:az ~len_a:asz ~pos_b:bz ~len_b:bsz () in
        (* TRANSLATE: 3軸全てが交差している場合は衝突していると判定する。 *)
        if iy && iz then Some (a_inf, b_inf) else None
      else
        None
